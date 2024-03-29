#ifndef COMPILER_H
#define COMPILER_H

#include "Common.h"
#include "Parser.h"

__SILENCE_LLVM_WARNINGS_START
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"
__SILENCE_LLVM_WARNINGS_END

namespace qwip {

class Compiler {
 public:
  Compiler(const Diagnostic &diag) : diag_(diag) {}

  bool CompileModule(const Module &module);
  llvm::Module &getLLVMModule() const {
    assert_ptr(llvm_module_);
    return *llvm_module_;
  }
  const llvm::DataLayout &getLLVMDataLayout() const {
    return getLLVMModule().getDataLayout();
  }
  bool SaveToExecutable(const std::string &input_filename,
                        const std::string &output_file);

 private:
  bool CompileExternDecl(const ExternDecl &decl);

#define EXTERN_DECL(Kind, Class) bool Compile##Class(const Class &extern_decl);
#include "Nodes.def"

  bool CompileStmt(const Stmt &stmt, llvm::IRBuilder<> &builder);
  bool CompileStmts(const std::vector<std::unique_ptr<Stmt>> &stmts,
                    llvm::IRBuilder<> &builder);
#define STMT(Kind, Class) \
  bool Compile##Class(const Class &stmt, llvm::IRBuilder<> &builder);
#include "Nodes.def"

  bool CompileExpr(const Expr &expr, llvm::IRBuilder<> &builder,
                   llvm::Value *&result);
#define EXPR(Kind, Class)                                            \
  bool Compile##Class(const Class &expr, llvm::IRBuilder<> &builder, \
                      llvm::Value *&result);
#include "Nodes.def"

  llvm::Type *toLLVMType(const Type &type);
#define TYPE(Kind, Class) llvm::Type *Class##ToLLVMType(const Class &type);
#include "Types.def"

  llvm::Value *getAddrOfVariable(const std::string &name, llvm::Function *func);
  llvm::Value *getAddrOfMemberAccess(const MemberAccess &expr,
                                     llvm::IRBuilder<> &builder);
  llvm::Value *getAddrOfSubscript(const Subscript &expr,
                                  llvm::IRBuilder<> &builder);
  bool CompileVarDecl(const VarDecl &decl, llvm::IRBuilder<> &builder,
                      llvm::Value *&alloca);

  llvm::Constant *getZero(llvm::Type *ty) {
    return llvm::Constant::getNullValue(ty);
  }
  llvm::Type *getInt32Ty() { return llvm::Type::getInt32Ty(llvm_context_); }

  const Diagnostic diag_;
  llvm::LLVMContext llvm_context_;
  std::unique_ptr<llvm::Module> llvm_module_;
};

}  // namespace qwip

#endif
