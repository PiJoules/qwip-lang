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

llvm::Function *getPrintFunction(llvm::Module &module) {
  if (llvm::GlobalValue *printf_func = module.getNamedValue("print"))
    return llvm::cast<llvm::Function>(printf_func);

  llvm::LLVMContext &context = module.getContext();
  auto *printf_type = llvm::FunctionType::get(
      llvm::Type::getInt32Ty(context), {llvm::Type::getInt8PtrTy(context)},
      /*isVarArg=*/true);
  llvm::Function *printf_func = llvm::Function::Create(
      printf_type, llvm::GlobalValue::ExternalLinkage, "printf", &module);

  auto *print_type = llvm::FunctionType::get(
      llvm::Type::getVoidTy(context), {llvm::Type::getInt8PtrTy(context)},
      /*isVarArg=*/false);
  llvm::Function *print_func = llvm::Function::Create(
      print_type, llvm::GlobalValue::ExternalLinkage, "print", &module);

  // Create the body.
  llvm::BasicBlock *entry =
      llvm::BasicBlock::Create(context, "entry", print_func);
  llvm::IRBuilder<> builder(entry);
  llvm::Value *first_arg = print_func->arg_begin();
  llvm::CallInst *tailcall = builder.CreateCall(printf_func, {first_arg});
  tailcall->setTailCall();
  builder.CreateRetVoid();

  return print_func;
}

typedef llvm::Function *(*FunctionMaker)(llvm::Module &);
const std::unordered_map<std::string, FunctionMaker> kBuiltinFunctions = {
    {"print", getPrintFunction},
};

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
  if (decl.getKind() == NODE_FUNCDEF)
    return CompileFuncDef(decl.getAs<FuncDef>());

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
  return llvm::FunctionType::get(ret_type, arg_types, /*isVarArg=*/false);
}

llvm::Type *Compiler::IDTypeToLLVMType(const IDType &type) {
  if (type.getName() == "i32")
    return llvm::Type::getInt32Ty(llvm_context_);
  else if (type.getName() == "i8")
    return llvm::Type::getInt8Ty(llvm_context_);
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

bool Compiler::CompileID(const ID &expr, llvm::IRBuilder<> &builder,
                         llvm::Value *&result) {
  auto found_builtin = kBuiltinFunctions.find(expr.getName());
  if (found_builtin != kBuiltinFunctions.end()) {
    FunctionMaker func_maker = found_builtin->second;
    result = func_maker(getLLVMModule());
    return true;
  }

  if (llvm::GlobalValue *val = getLLVMModule().getNamedValue(expr.getName()))
    return val;

  llvm::Function *func = builder.GetInsertBlock()->getParent();
  const auto *local_symbols = func->getValueSymbolTable();
  result = local_symbols->lookup(expr.getName());
  if (!result)
    std::cerr << "Unable to find symbol for '" << expr.getName() << "'\n";
  return result;
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
  result = global_str;
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

bool Compiler::SaveToExecutable(const std::string &output_filename) {
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
  std::string object_filename = "tmp.o";
  llvm::raw_fd_ostream dest(object_filename, err_code, llvm::sys::fs::F_None);
  if (err_code) {
    std::cerr << "Could not open file: " << err_code.message();
    return false;
  }

  llvm::legacy::PassManager pass;
  if (TheTargetMachine->addPassesToEmitFile(
          pass, dest, llvm::TargetMachine::CGFT_ObjectFile)) {
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
      llvm::sys::ExecuteAndWait(cc_binary, args.data(), /*Env=*/nullptr,
                                /*Redirects=*/{},
                                /*secondsToWait*/ 0,
                                /*memoryLimit*/ 0, &err_msg);
  if (result) std::cerr << err_msg << "\n";

  return result == 0;
}

}  // namespace qwip
