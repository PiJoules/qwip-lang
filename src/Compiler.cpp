#include "Compiler.h"

#include <functional>
#include <iostream>
#include <unordered_map>

#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/Support/FileSystem.h"
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
    case NODE_FUNCDEF:
      return CompileFuncDef(decl.getAs<FuncDef>());
    case NODE_EXTERN_VARDECL:
      return CompileExternVarDecl(decl.getAs<ExternVarDecl>());

#define EXTERN_DECL(Kind, Class)
#define NODE(Kind, Class) case Kind:
#include "Nodes.def"
      UNREACHABLE("Unhandled External declaration");
      return false;
  }

  std::cerr << "Can only compile function definitions for now.\n";
  return false;
}

llvm::Type *Compiler::toLLVMType(const Type &type) {
  switch (type.getKind()) {
#define TYPE(Kind, Class) \
  case Kind:              \
    return Class##ToLLVMType(type.getAs<Class>());
#include "Types.def"
  }
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
  if (name == "void") {
    return llvm::Type::getVoidTy(llvm_context_);
  }

  std::cerr << "Unable to create an LLVM type from " << type.getName() << "\n";
  return nullptr;
}

llvm::Type *Compiler::PtrTypeToLLVMType(const PtrType &type) {
  return llvm::PointerType::getUnqual(toLLVMType(type.getPointeeType()));
}

bool Compiler::CompileFuncDecl(const FuncDecl &funcdecl,
                               llvm::Function *&result) {
  const TypeNode &type_node = funcdecl.getTypeNode();
  CHECK(type_node.getKind() == NODE_FUNC_TYPE, "Expected a function type node");
  const auto &func_type_node = type_node.getAs<FuncTypeNode>();
  auto func_type = func_type_node.toType();
  auto *llvm_func_type = llvm::cast<llvm::FunctionType>(toLLVMType(*func_type));
  result =
      llvm::Function::Create(llvm_func_type, llvm::Function::ExternalLinkage,
                             funcdecl.getName(), llvm_module_.get());

  // Assign the argument names.
  const auto &params = func_type_node.getParams();
  unsigned Idx = 0;
  for (auto &arg : result->args()) {
    arg.setName(params[Idx++]->getName());
  }

  return true;
}

bool Compiler::CompileExternVarDecl(const ExternVarDecl &extern_decl) {
  const VarDecl &decl = extern_decl.getDecl();
  if (decl.getKind() == NODE_FUNCDECL) {
    llvm::Function *result;
    return CompileFuncDecl(decl.getAs<FuncDecl>(), result);
  }

  std::unique_ptr<Type> type = decl.getType();
  llvm::Type *llvm_type = toLLVMType(*type);

  return getLLVMModule().getOrInsertGlobal(
      decl.getName(), llvm_type, [&]() -> llvm::GlobalVariable * {
        llvm::Constant *init;
        if (decl.hasInit()) {
          llvm::Value *init_val;
          llvm::IRBuilder<> builder(llvm_context_);
          if (!CompileExpr(decl.getInit(), builder, init_val)) return nullptr;
          init = llvm::dyn_cast<llvm::Constant>(init_val);
          if (!init) {
            std::cerr
                << "The initial value of this global is not a constant.\n";
            return nullptr;
          }
        } else {
          init = llvm::Constant::getNullValue(llvm_type);
        }

        return new llvm::GlobalVariable(
            getLLVMModule(), llvm_type, /*isConstant=*/true,
            llvm::GlobalValue::ExternalLinkage, init, decl.getName());
      });
}

bool Compiler::CompileFuncDef(const FuncDef &funcdef) {
  llvm::Function *func;
  if (!CompileFuncDecl(funcdef.getDecl(), func)) return false;

  llvm::BasicBlock *entry_block =
      llvm::BasicBlock::Create(llvm_context_, "entry", func);
  llvm::IRBuilder<> builder(llvm_context_);
  builder.SetInsertPoint(entry_block);

  for (const auto &stmt_ptr : funcdef.getStmts()) {
    if (!CompileStmt(*stmt_ptr, builder)) return false;
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
    UNREACHABLE("Unknown expression kind.");
    return false;
  }
}

bool Compiler::CompileInt(const Int &expr, llvm::IRBuilder<> &builder,
                          llvm::Value *&result) {
  result = llvm::ConstantInt::get(llvm_context_,
                                  llvm::APInt(/*num_bits=*/64, expr.getVal(),
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

  builder.CreateCall(caller, args);
  return true;
}

bool Compiler::CompileReturn(const Return &stmt, llvm::IRBuilder<> &builder) {
  llvm::Value *ret_val;
  if (!CompileExpr(stmt.getExpr(), builder, ret_val)) return false;
  builder.CreateRet(ret_val);
  return true;
}

bool Compiler::CompileCallStmt(const CallStmt &stmt,
                               llvm::IRBuilder<> &builder) {
  llvm::Value *ret_val;
  if (!CompileCall(stmt.getCall(), builder, ret_val)) return false;
  return true;
}

bool Compiler::CompileAssign(const Assign &stmt, llvm::IRBuilder<> &builder) {
  llvm::Value *value_addr;
  switch (stmt.getLHS().getKind()) {
    case NODE_ID: {
      const auto &id_expr = stmt.getLHS().getAs<ID>();
      value_addr = getAddrOfVariable(id_expr.getName(),
                                     builder.GetInsertBlock()->getParent());
      break;
    }
#define ASSIGNABLE(Kind, Class)
#define NODE(Kind, Class) case Kind:
#include "Nodes.def"
      UNREACHABLE("Unhandled assignable kind.");
      return false;
  }
  llvm::Value *init;
  if (!CompileExpr(stmt.getExpr(), builder, init)) return false;
  builder.CreateStore(init, value_addr, /*isVolatile=*/false);
  return true;
}

bool Compiler::CompileVarDecl(const VarDecl &stmt, llvm::IRBuilder<> &builder) {
  std::unique_ptr<Type> type = stmt.getType();
  llvm::Type *llvm_type = toLLVMType(*type);
  llvm::Value *value_addr =
      builder.CreateAlloca(llvm_type, /*ArraySize=*/nullptr,
                           /*Name=*/stmt.getName());
  if (stmt.hasInit()) {
    llvm::Value *init;
    if (!CompileExpr(stmt.getInit(), builder, init)) return false;
    builder.CreateStore(init, value_addr, /*isVolatile=*/false);
  }
  return true;
}

bool Compiler::SaveToExecutable(const std::string &input_filename,
                                const std::string &output_filename) {
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
    std::cerr << "Failed to lookup target " + TargetTriple + ": " + err;
    return false;
  }

  llvm::TargetOptions opt;
  std::unique_ptr<llvm::TargetMachine> TheTargetMachine(
      Target->createTargetMachine(TargetTriple, "generic", "", opt,
                                  llvm::Optional<llvm::Reloc::Model>()));

  llvm_module_->setTargetTriple(TargetTriple);
  llvm_module_->setDataLayout(TheTargetMachine->createDataLayout());

  std::error_code err_code;
  std::string object_filename = input_filename + ".o";
  llvm::raw_fd_ostream dest(object_filename, err_code, llvm::sys::fs::F_None);
  if (err_code) {
    std::cerr << "Could not open file: " << err_code.message();
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
  args.push_back(output_filename.c_str());
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
