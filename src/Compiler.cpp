#include "Compiler.h"

#include <functional>
#include <iostream>
#include <unordered_map>

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"

namespace qwip {

namespace {

bool FindExe(const std::string &exe_name, std::string &linker_path) {
  auto result = llvm::sys::findProgramByName(exe_name);
  linker_path = result.get();
  return !result.getError();
}

}  // namespace

bool Compiler::CompileModule(const Module &module) {
  llvm_module_ = std::make_unique<llvm::Module>("__main__", llvm_context_);

  for (const auto &extern_decl_ptr : module.getDecls()) {
    if (!CompileExternDecl(*extern_decl_ptr)) return false;
  }

  return true;
}

bool Compiler::CompileExternDecl(const ExternDecl &decl) {
  switch (decl.getKind()) {
#define EXTERN_DECL(Kind, Class) \
  case Kind:                     \
    return Compile##Class(decl.getAs<Class>());
#define NODE(Kind, Class) case Kind:
#include "Nodes.def"
    break;
  }
  UNREACHABLE("Unhandled External declaration");
  return false;
}

llvm::Type *Compiler::toLLVMType(const Type &type) {
  llvm::Type *llvm_type = nullptr;
  switch (type.getKind()) {
#define TYPE(Kind, Class)                               \
  case Kind:                                            \
    llvm_type = Class##ToLLVMType(type.getAs<Class>()); \
    break;
#include "Types.def"
  }
  assert(llvm_type && "Unsuccessful conversion to an llvm type.");
  return llvm_type;
}

llvm::Type *Compiler::FuncTypeToLLVMType(const FuncType &type) {
  llvm::Type *ret_type = toLLVMType(type.getReturnType());
  std::vector<llvm::Type *> arg_types;
  for (const auto &type_ptr : type.getArgTypes()) {
    llvm::Type *arg_type = toLLVMType(*type_ptr);
    arg_types.push_back(arg_type);
  }
  return llvm::FunctionType::get(ret_type, arg_types,
                                 /*isVarArg=*/type.isVarArg());
}

llvm::Type *Compiler::IDTypeToLLVMType(const IDType &type) {
  const std::string &name = type.getName();
  if (name[0] == 'i') {
    // This could be an int.
    const std::string int_part(name.begin() + 1, name.end());
    unsigned size = std::stoul(int_part);
    return llvm::IntegerType::get(llvm_context_, size);
  }
  if (name == "void") return llvm::Type::getVoidTy(llvm_context_);
  if (llvm::StructType *named_type = getLLVMModule().getTypeByName(name))
    return named_type;

  std::cerr << "Unable to create an LLVM type from " << type.getName() << "\n";
  return nullptr;
}

llvm::Type *Compiler::IntTypeToLLVMType(const IntType &type) {
  return llvm::IntegerType::get(llvm_context_, type.getNumBits());
}

llvm::Type *Compiler::StrTypeToLLVMType(const StrType &type) {
  return llvm::ArrayType::get(llvm::Type::getInt8Ty(llvm_context_),
                              type.getSize());
}

llvm::Type *Compiler::StructTypeToLLVMType(const StructType &type) {
  std::vector<llvm::Type *> llvm_types;
  for (const auto &type_ptr : type.getTypes()) {
    llvm_types.push_back(toLLVMType(*type_ptr));
  }
  return llvm::StructType::get(llvm_context_, llvm_types);
}

llvm::Type *Compiler::PtrTypeToLLVMType(const PtrType &type) {
  return llvm::PointerType::getUnqual(toLLVMType(type.getPointeeType()));
}

bool Compiler::CompileVarDecl(const VarDecl &decl, llvm::IRBuilder<> &builder,
                              llvm::Value *&result) {
  std::unique_ptr<Type> type = decl.getType();
  llvm::Type *llvm_type = toLLVMType(*type);

  if (auto *llvm_func_type = llvm::dyn_cast<llvm::FunctionType>(llvm_type)) {
    result =
        llvm::Function::Create(llvm_func_type, llvm::Function::ExternalLinkage,
                               decl.getName(), llvm_module_.get());
  } else {
    result = builder.CreateAlloca(llvm_type,
                                  /*ArraySize=*/nullptr, decl.getName());
  }
  return true;
}

bool Compiler::CompileExternTypeDef(const ExternTypeDef &extern_typedef) {
  llvm::IRBuilder<> builder(llvm_context_);
  return CompileTypeDef(extern_typedef.getTypeDef(), builder);
}

bool Compiler::CompileTypeDef(const TypeDef &type_def,
                              llvm::IRBuilder<> &builder) {
  std::vector<llvm::Type *> llvm_types;
  for (const auto &member_ptr : type_def.getMembers()) {
    std::unique_ptr<Type> type;
    switch (member_ptr->getKind()) {
      case NODE_MEMBER_VARDECL: {
        const auto &member = member_ptr->getAs<MemberVarDecl>();
        type = member.getVarDecl().getType();
        if (type->getKind() == TYPE_FUNC) {
          llvm_types.push_back(llvm::PointerType::getUnqual(toLLVMType(*type)));
        } else {
          llvm_types.push_back(toLLVMType(*type));
        }
        break;
      }
#define MEMBER_DECL(Kind, Class)
#define NODE(Kind, Class) case Kind:
#include "Nodes.def"
        UNREACHABLE("Unexpected member decl.");
        return false;
    }
  }

  llvm::StructType::create(llvm_context_, llvm_types, type_def.getName());
  return true;
}

bool Compiler::CompileExternVarDecl(const ExternVarDecl &extern_decl) {
  const VarDecl &decl = extern_decl.getVarDecl();
  const auto &typenode = decl.getTypeNode();
  if (typenode.getKind() == NODE_FUNC_TYPE) {
    llvm::IRBuilder<> builder(llvm_context_);
    return CompileVarDecl(decl, builder);
  }

  std::unique_ptr<Type> type = decl.getType();
  llvm::Type *llvm_type = toLLVMType(*type);
  llvm::GlobalVariable *global = llvm::dyn_cast_or_null<llvm::GlobalVariable>(
      getLLVMModule().getNamedValue(decl.getName()));
  if (!global) {
    llvm::Constant *init = llvm::Constant::getNullValue(llvm_type);
    global = new llvm::GlobalVariable(
        getLLVMModule(), llvm_type, /*isConstant=*/true,
        llvm::GlobalValue::ExternalLinkage, init, decl.getName());
  }
  return global;
}

bool Compiler::CompileExternVarDef(const ExternVarDef &externdef) {
  const VarDef &def = externdef.getVarDef();
  const VarDecl &decl = def.getDecl();
  std::unique_ptr<Type> type = decl.getType();
  llvm::Type *llvm_type = toLLVMType(*type);

  llvm::GlobalVariable *global = llvm::dyn_cast_or_null<llvm::GlobalVariable>(
      getLLVMModule().getNamedValue(decl.getName()));
  if (!global) {
    llvm::Constant *init;
    llvm::Value *init_val;
    llvm::IRBuilder<> builder(llvm_context_);
    if (!CompileExpr(def.getInit(), builder, init_val)) return false;

    init = llvm::dyn_cast<llvm::Constant>(init_val);
    if (!init) {
      std::cerr << "The initial value of this global is not a constant.\n";
      return false;
    }

    return new llvm::GlobalVariable(
        getLLVMModule(), llvm_type, /*isConstant=*/true,
        llvm::GlobalValue::ExternalLinkage, init, decl.getName());
  }
  return global;
}

bool Compiler::CompileStmts(const std::vector<std::unique_ptr<Stmt>> &stmts,
                            llvm::IRBuilder<> &builder) {
  for (const auto &stmt_ptr : stmts) {
    if (!CompileStmt(*stmt_ptr, builder)) return false;
  }
  return true;
}

bool Compiler::CompileExternFuncDef(const ExternFuncDef &extern_funcdef) {
  llvm::IRBuilder<> builder(llvm_context_);
  return CompileFuncDef(extern_funcdef.getFuncDef(), builder);
}

bool Compiler::CompileFuncDef(const FuncDef &funcdef, llvm::IRBuilder<> &) {
  llvm::Value *allocation;
  llvm::IRBuilder<> builder(llvm_context_);
  if (!CompileVarDecl(funcdef.getDecl(), builder, allocation)) return false;
  auto *func = llvm::cast<llvm::Function>(allocation);

  llvm::BasicBlock *entry_block =
      llvm::BasicBlock::Create(llvm_context_, "entry", func);
  builder.SetInsertPoint(entry_block);

  // Store the arguments.
  const TypeNode &type_node = funcdef.getDecl().getTypeNode();
  assert(type_node.getKind() == NODE_FUNC_TYPE &&
         "Expected a function type node");
  const auto &func_type_node = type_node.getAs<FuncTypeNode>();
  const auto &params = func_type_node.getParams();
  for (auto &arg : func->args()) {
    llvm::Value *value_addr =
        builder.CreateAlloca(arg.getType(), /*ArraySize=*/nullptr,
                             params[arg.getArgNo()]->getName());
    builder.CreateStore(&arg, value_addr, /*isVolatile=*/false);
  }

  if (!CompileStmts(funcdef.getStmts(), builder)) return false;

  if (!entry_block->getTerminator()) {
    // Return null by default.
    std::cerr << "No explicit return provided. Returning 0.\n";
    builder.CreateRet(llvm::Constant::getNullValue(func->getReturnType()));
  }

  return true;
}

bool Compiler::CompileStmt(const Stmt &stmt, llvm::IRBuilder<> &builder) {
  switch (stmt.getKind()) {
#define NODE(Kind, Class)                              \
  case Kind:                                           \
    std::cerr << "Invalid stmt kind: " STR(Kind) "\n"; \
    return false;
#define STMT(Kind, Class) \
  case Kind:              \
    return Compile##Class(stmt.getAs<Class>(), builder);
#include "Nodes.def"
  }
  UNREACHABLE("Unhandled statement kind");
  return false;
}

bool Compiler::CompileExpr(const Expr &expr, llvm::IRBuilder<> &builder,
                           llvm::Value *&result) {
  switch (expr.getKind()) {
#define NODE(Kind, Class)
#define EXPR(Kind, Class) \
  case Kind:              \
    return Compile##Class(expr.getAs<Class>(), builder, result);
#include "Nodes.def"

#define EXPR(Kind, Class)
#define NODE(Kind, Class) case Kind:
#include "Nodes.def"
    break;
  }
  UNREACHABLE("Unhandled expression kind");
  return false;
}

bool Compiler::CompileInt(const Int &expr, llvm::IRBuilder<> &builder,
                          llvm::Value *&result) {
  result = llvm::ConstantInt::get(llvm_context_,
                                  llvm::APInt(/*num_bits=*/32, expr.getVal(),
                                              /*isSigned=*/true));
  return true;
}

llvm::Value *Compiler::getAddrOfVariable(const std::string &name,
                                         llvm::Function *func) {
  // Check locals first.
  const auto *local_symbols = func->getValueSymbolTable();
  llvm::Value *result = local_symbols->lookup(name);
  if (result) return result;

  // Then check globals.
  if (llvm::GlobalValue *val = getLLVMModule().getNamedValue(name)) return val;

  return nullptr;
}

bool Compiler::CompileID(const ID &expr, llvm::IRBuilder<> &builder,
                         llvm::Value *&result) {
  llvm::Value *var_addr =
      getAddrOfVariable(expr.getName(), builder.GetInsertBlock()->getParent());
  if (!var_addr) {
    std::cerr << "Unable to find symbol for '" << expr.getName() << "'\n";
    return false;
  }

  const auto *ptr_type = llvm::cast<llvm::PointerType>(var_addr->getType());
  if (ptr_type->getElementType()->isFunctionTy()) {
    result = var_addr;
  } else {
    result = builder.CreateLoad(var_addr);
  }
  return true;
}

llvm::Value *Compiler::getAddrOfMemberAccess(const MemberAccess &expr,
                                             llvm::IRBuilder<> &builder) {
  llvm::Value *base_val;
  if (!CompileExpr(expr.getBase(), builder, base_val)) return nullptr;

  // If the base is an ID, it will be loaded as a value when we want it's
  // address. If it is a LoadInst, we can just get the pointer operand.
  if (auto *load = llvm::dyn_cast<llvm::LoadInst>(base_val))
    base_val = load->getPointerOperand();

  const Expr &base = expr.getBase();
  std::unique_ptr<Type> base_type = base.getType();

  // The base is guaranteed to be a StructType.
  const auto &struct_type = base_type->getAs<StructType>();

  auto *i32ty = llvm::Type::getInt32Ty(llvm_context_);
  auto *zero = llvm::Constant::getNullValue(i32ty);
  auto idx =
      llvm::ConstantInt::get(i32ty, struct_type.getIndex(expr.getMember()));
  return builder.CreateInBoundsGEP(base_val, {zero, idx});
}

bool Compiler::CompileMemberAccess(const MemberAccess &expr,
                                   llvm::IRBuilder<> &builder,
                                   llvm::Value *&result) {
  if (!(result = getAddrOfMemberAccess(expr, builder))) return false;
  result = builder.CreateLoad(result);
  return true;
}

bool Compiler::CompileBinOp(const BinOp &expr, llvm::IRBuilder<> &builder,
                            llvm::Value *&result) {
  llvm::Value *lhs_val, *rhs_val;
  if (!CompileExpr(expr.getLHS(), builder, lhs_val)) return false;
  if (!CompileExpr(expr.getRHS(), builder, rhs_val)) return false;

  switch (expr.getBinOp()) {
    case BINOP_LT:
      result = builder.CreateICmpSLT(lhs_val, rhs_val);
      break;
    case BINOP_LE:
      result = builder.CreateICmpSLE(lhs_val, rhs_val);
      break;
    case BINOP_SUB:
      result = builder.CreateSub(lhs_val, rhs_val);
      break;
    case BINOP_ADD:
      result = builder.CreateAdd(lhs_val, rhs_val);
      break;
  }

  return true;
}

bool Compiler::CompileStr(const Str &expr, llvm::IRBuilder<> &builder,
                          llvm::Value *&result) {
  llvm::Constant *str =
      llvm::ConstantDataArray::getString(llvm_context_, expr.getVal(),
                                         /*AddNull=*/true);
  auto *global_str = new llvm::GlobalVariable(
      getLLVMModule(), str->getType(), /*isConstant=*/true,
      llvm::GlobalValue::PrivateLinkage, str);
  global_str->setUnnamedAddr(llvm::GlobalValue::UnnamedAddr::Global);

  // Use GEP to convert the char array to a char pointer.
  llvm::Value *zero =
      llvm::Constant::getNullValue(llvm::Type::getInt32Ty(llvm_context_));
  result = builder.CreateGEP(global_str, {zero, zero});
  return true;
}

bool Compiler::CompileCall(const Call &expr, llvm::IRBuilder<> &builder,
                           llvm::Value *&result) {
  llvm::Value *caller;
  if (!CompileExpr(expr.getCaller(), builder, caller)) return false;

  std::vector<llvm::Value *> args;
  for (const auto &arg_ptr : expr.getArgs()) {
    llvm::Value *llvm_arg;
    if (!CompileExpr(*arg_ptr, builder, llvm_arg)) return false;
    args.push_back(llvm_arg);
  }

  result = builder.CreateCall(caller, args);
  return true;
}

bool Compiler::CompileReturn(const Return &stmt, llvm::IRBuilder<> &builder) {
  llvm::Value *ret_val;
  if (!CompileExpr(stmt.getExpr(), builder, ret_val)) return false;
  builder.CreateRet(ret_val);
  return true;
}

bool Compiler::CompileWhile(const While &stmt, llvm::IRBuilder<> &builder) {
  llvm::Function *func = builder.GetInsertBlock()->getParent();
  llvm::BasicBlock *loop_startbb =
      llvm::BasicBlock::Create(llvm_context_, "loop_start", func);
  llvm::BasicBlock *loopbb = llvm::BasicBlock::Create(llvm_context_, "loop");
  llvm::BasicBlock *loop_endbb =
      llvm::BasicBlock::Create(llvm_context_, "loop_exit");

  builder.CreateBr(loop_startbb);
  builder.SetInsertPoint(loop_startbb);

  llvm::Value *cond_val;
  if (!CompileExpr(stmt.getCond(), builder, cond_val)) return false;
  llvm::Constant *zero = llvm::Constant::getNullValue(cond_val->getType());
  cond_val = builder.CreateICmpNE(cond_val, zero);

  builder.CreateCondBr(cond_val, loopbb, loop_endbb);

  // Emit the then block.
  func->getBasicBlockList().push_back(loopbb);
  builder.SetInsertPoint(loopbb);
  if (!CompileStmts(stmt.getBody(), builder)) return false;
  builder.CreateBr(loop_startbb);

  // Emit the merge block.
  func->getBasicBlockList().push_back(loop_endbb);
  builder.SetInsertPoint(loop_endbb);

  return true;
}

bool Compiler::CompileIf(const If &stmt, llvm::IRBuilder<> &builder) {
  llvm::Value *cond_val;
  if (!CompileExpr(stmt.getCond(), builder, cond_val)) return false;
  llvm::Constant *zero = llvm::Constant::getNullValue(cond_val->getType());
  cond_val = builder.CreateICmpNE(cond_val, zero);

  llvm::Function *func = builder.GetInsertBlock()->getParent();
  llvm::BasicBlock *thenbb =
      llvm::BasicBlock::Create(llvm_context_, "then", func);
  llvm::BasicBlock *elsebb = llvm::BasicBlock::Create(llvm_context_, "else");
  llvm::BasicBlock *mergebb = llvm::BasicBlock::Create(llvm_context_, "merge");

  builder.CreateCondBr(cond_val, thenbb, mergebb);

  // Emit the then block.
  builder.SetInsertPoint(thenbb);
  if (!CompileStmts(stmt.getBody(), builder)) return false;
  builder.CreateBr(mergebb);

  // Emit the else block.
  func->getBasicBlockList().push_back(elsebb);
  builder.SetInsertPoint(elsebb);
  if (!CompileStmts(stmt.getElseBody(), builder)) return false;
  builder.CreateBr(mergebb);

  // Emit the merge block.
  func->getBasicBlockList().push_back(mergebb);
  builder.SetInsertPoint(mergebb);

  return true;
}

bool Compiler::CompileCallStmt(const CallStmt &stmt,
                               llvm::IRBuilder<> &builder) {
  llvm::Value *ret_val;
  if (!CompileCall(stmt.getCall(), builder, ret_val)) return false;
  return true;
}

bool Compiler::CompileAssign(const Assign &stmt, llvm::IRBuilder<> &builder) {
  llvm::Value *init;
  if (!CompileExpr(stmt.getExpr(), builder, init)) return false;

  llvm::Value *value_addr = nullptr;
  switch (stmt.getLHS().getKind()) {
    case NODE_ID: {
      const auto &id_expr = stmt.getLHS().getAs<ID>();
      value_addr = getAddrOfVariable(id_expr.getName(),
                                     builder.GetInsertBlock()->getParent());
      break;
    }
    case NODE_MEMBER_ACCESS: {
      const auto &member = stmt.getLHS().getAs<MemberAccess>();
      if (!(value_addr = getAddrOfMemberAccess(member, builder))) return false;
      break;
    }
#define ASSIGNABLE(Kind, Class)
#define NODE(Kind, Class) case Kind:
#include "Nodes.def"
      UNREACHABLE("Unhandled assignable kind.");
      return false;
  }
  builder.CreateStore(init, value_addr, /*isVolatile=*/false);
  return true;
}

bool Compiler::CompileVarDecl(const VarDecl &stmt, llvm::IRBuilder<> &builder) {
  llvm::Value *result;
  return CompileVarDecl(stmt, builder, result);
}

bool Compiler::CompileVarDef(const VarDef &stmt, llvm::IRBuilder<> &builder) {
  llvm::Value *value_addr;
  if (!CompileVarDecl(stmt.getDecl(), builder, value_addr)) return false;
  llvm::Value *init;
  if (!CompileExpr(stmt.getInit(), builder, init)) return false;
  builder.CreateStore(init, value_addr, /*isVolatile=*/false);
  return true;
}

bool Compiler::SaveToExecutable(const std::string &input_filepath,
                                const std::string &output_filepath) {
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmParsers();
  llvm::InitializeAllAsmPrinters();

  std::string TargetTriple = llvm::sys::getDefaultTargetTriple();

  std::string err;
  const llvm::Target *Target =
      llvm::TargetRegistry::lookupTarget(TargetTriple, err);
  if (!Target) {
    std::cerr << "Failed to lookup target " << TargetTriple << ": " << err
              << "\n";
    return false;
  }

  llvm::TargetOptions opt;
  std::unique_ptr<llvm::TargetMachine> TheTargetMachine(
      Target->createTargetMachine(TargetTriple, "generic", "", opt,
                                  llvm::Optional<llvm::Reloc::Model>()));

  llvm_module_->setTargetTriple(TargetTriple);
  llvm_module_->setDataLayout(TheTargetMachine->createDataLayout());

  std::error_code err_code;
  llvm::SmallString<128> working_dir;
  err_code = llvm::sys::fs::createUniqueDirectory("qwip-output", working_dir);
  if (err_code) {
    std::cerr
        << "Could not create a working directory to store temporary files: "
        << err_code.message() << "\n";
    return false;
  }

  std::string input_filename = llvm::sys::path::filename(input_filepath);
  llvm::SmallString<128> object_filename(working_dir);
  llvm::sys::path::append(object_filename, input_filename + ".o");
  llvm::raw_fd_ostream dest(object_filename, err_code, llvm::sys::fs::F_None);
  if (err_code) {
    std::cerr << "Could not open file (" << object_filename.c_str()
              << "): " << err_code.message() << "\n";
    return false;
  }

  llvm::legacy::PassManager pass;
  if (TheTargetMachine->addPassesToEmitFile(
          pass, dest, /*DwoOut=*/nullptr,
          llvm::TargetMachine::CGFT_ObjectFile)) {
    std::cerr << "TheTargetMachine can't emit a file of this type\n";
    return false;
  }
  pass.run(*llvm_module_);
  dest.flush();
  dest.close();

  std::string cc_binary;
  if (!FindExe("clang++", cc_binary)) {
    std::cerr << "Couldn't find a c++ compiler.\n";
    return false;
  }

  llvm::SmallVector<const char *, 4> args;
  args.push_back(cc_binary.c_str());
  args.push_back(object_filename.c_str());
  args.push_back("-o");
  args.push_back(output_filepath.c_str());
  args.push_back(nullptr);
  std::string err_msg;

  int result =
      llvm::sys::ExecuteAndWait(cc_binary, llvm::toStringRefArray(args.data()),
                                /*Env=*/llvm::None,
                                /*Redirects=*/{},
                                /*secondsToWait*/ 0,
                                /*memoryLimit*/ 0, &err_msg);
  if (result) std::cerr << err_msg << "\n";

  return result == 0;
}

}  // namespace qwip
