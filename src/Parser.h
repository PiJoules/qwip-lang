#ifndef PARSER_H
#define PARSER_H

#include <memory>
#include <vector>

#include "Diagnostics.h"
#include "Lexer.h"

namespace qwip {

enum NodeKind {
#define NODE(Kind, Class) Kind,
#include "Nodes.def"
};

#define CHECK_PTR(PTR) CHECK(bool(PTR), "Expected a  non-null pointer.")

template <typename T>
void __CheckUptrVector(const std::vector<std::unique_ptr<T>> &ptrs) {
  for (const auto &ptr : ptrs) CHECK_PTR(ptr);
}
#define CHECK_PTRS(PTRS) __CheckUptrVector(PTRS)

enum TypeKind {
#define TYPE(Kind, Class) Kind,
#include "Types.def"
};

class Type {
 public:
  virtual ~Type() {}
  virtual TypeKind getKind() const = 0;

  template <typename T>
  const T &getAs() const {
    return static_cast<const T &>(*this);
  }
};

class FuncType : public Type {
 public:
  FuncType(std::unique_ptr<Type> &ret_type,
           std::vector<std::unique_ptr<Type>> &arg_types, bool isvararg)
      : ret_type_(std::move(ret_type)),
        arg_types_(std::move(arg_types)),
        isvararg_(isvararg) {
    CHECK_PTR(ret_type_);
    CHECK_PTRS(arg_types_);
  }

  TypeKind getKind() const override { return TYPE_FUNC; }
  const Type &getReturnType() const { return *ret_type_; }
  const std::vector<std::unique_ptr<Type>> &getArgTypes() const {
    return arg_types_;
  }
  bool isVarArg() const { return isvararg_; }

 private:
  std::unique_ptr<Type> ret_type_;
  std::vector<std::unique_ptr<Type>> arg_types_;
  bool isvararg_;
};

class IDType : public Type {
 public:
  IDType(const std::string &name) : name_(name) {}

  TypeKind getKind() const override { return TYPE_ID; }
  const std::string &getName() const { return name_; }

 private:
  std::string name_;
};

class PtrType : public Type {
 public:
  PtrType(std::unique_ptr<Type> &pointee) : pointee_(std::move(pointee)) {
    CHECK_PTR(pointee_);
  }

  TypeKind getKind() const override { return TYPE_PTR; }
  const Type &getPointeeType() const { return *pointee_; }

 private:
  std::unique_ptr<Type> pointee_;
};

class Node {
 public:
  Node(const SourceLocation loc) : loc_(loc) {}
  virtual ~Node() {}
  virtual NodeKind getKind() const = 0;
  const SourceLocation getLoc() const { return loc_; }

  template <typename T>
  const T &getAs() const {
    return static_cast<const T &>(*this);
  }

 private:
  SourceLocation loc_;
};

class ExternDecl : public Node {
 public:
  ExternDecl(const SourceLocation loc, const std::string &name)
      : Node(loc), name_(name) {}

  const std::string &getName() const { return name_; }

 private:
  std::string name_;
};

class Module : public Node {
 public:
  Module(const SourceLocation &loc,
         std::vector<std::unique_ptr<ExternDecl>> &decls)
      : Node(loc), decls_(std::move(decls)) {
    CHECK_PTRS(decls_);
  }

  NodeKind getKind() const override { return NODE_MODULE; }
  const std::vector<std::unique_ptr<ExternDecl>> &getDecls() const {
    return decls_;
  }

 private:
  std::vector<std::unique_ptr<ExternDecl>> decls_;
};

class Expr : public Node {
 public:
  Expr(const SourceLocation loc) : Node(loc) {}
};

class Call : public Expr {
 public:
  Call(std::unique_ptr<Expr> &caller, std::vector<std::unique_ptr<Expr>> &args)
      : Expr(caller->getLoc()),
        caller_(std::move(caller)),
        args_(std::move(args)) {}

  NodeKind getKind() const override { return NODE_CALL; }
  const Expr &getCaller() const { return *caller_; }
  const std::vector<std::unique_ptr<Expr>> &getArgs() const { return args_; }

 private:
  std::unique_ptr<Expr> caller_;
  std::vector<std::unique_ptr<Expr>> args_;
};

class Str : public Expr {
 public:
  Str(const SourceLocation loc, const std::string &str)
      : Expr(loc), val_(str) {}

  NodeKind getKind() const override { return NODE_STR; }
  const std::string &getVal() const { return val_; }

 private:
  std::string val_;
};

class Int : public Expr {
 public:
  Int(const SourceLocation loc, int i) : Expr(loc), val_(i) {}
  NodeKind getKind() const override { return NODE_INT; }

  int getVal() const { return val_; }

 private:
  int val_;
};

class ID : public Expr {
 public:
  ID(const SourceLocation loc, const std::string &name)
      : Expr(loc), name_(name) {}
  NodeKind getKind() const override { return NODE_ID; }

  const std::string &getName() const { return name_; }

 private:
  std::string name_;
};

enum BinOpCode {
  BINOP_LT,
  BINOP_LE,
  BINOP_ADD,
  BINOP_SUB,
};

class BinOp : public Expr {
 public:
  BinOp(std::unique_ptr<Expr> &lhs, std::unique_ptr<Expr> &rhs, BinOpCode op)
      : Expr(lhs->getLoc()),
        lhs_(std::move(lhs)),
        rhs_(std::move(rhs)),
        op_(op) {
    CHECK_PTR(lhs_);
    CHECK_PTR(rhs_);
  }

  NodeKind getKind() const override { return NODE_BINOP; }
  const Expr &getLHS() const { return *lhs_; }
  const Expr &getRHS() const { return *rhs_; }
  BinOpCode getBinOp() const { return op_; }

 private:
  std::unique_ptr<Expr> lhs_;
  std::unique_ptr<Expr> rhs_;
  BinOpCode op_;
};

/**
 * This node represents the usage of a type in code, not to be confused with
 * semantic Types.
 */
class TypeNode : public Node {
 public:
  TypeNode(const SourceLocation loc) : Node(loc) {}
  virtual std::unique_ptr<Type> toType() const = 0;
};

class PtrTypeNode : public TypeNode {
 public:
  PtrTypeNode(const SourceLocation loc, std::unique_ptr<TypeNode> &type)
      : TypeNode(loc), pointee_type_(std::move(type)) {
    CHECK_PTR(pointee_type_);
  }
  NodeKind getKind() const override { return NODE_PTR_TYPE; }

  const TypeNode &getPointeeTypeNode() const { return *pointee_type_; }
  std::unique_ptr<Type> toType() const override {
    auto pointee_type = pointee_type_->toType();
    std::unique_ptr<Type> ptr_type(new PtrType(pointee_type));
    return ptr_type;
  }

 private:
  std::unique_ptr<TypeNode> pointee_type_;
};

class IDTypeNode : public TypeNode {
 public:
  IDTypeNode(const SourceLocation loc, const std::string &name)
      : TypeNode(loc), name_(name) {}

  NodeKind getKind() const override { return NODE_ID_TYPE; }
  const std::string &getName() const { return name_; }
  std::unique_ptr<Type> toType() const override {
    return std::unique_ptr<Type>(new IDType(name_));
  }

 private:
  std::string name_;
};

class Stmt : public Node {
 public:
  Stmt(const SourceLocation loc) : Node(loc) {}
};

class VarDecl : public Stmt {
 public:
  VarDecl(const SourceLocation loc, const std::string &name,
          std::unique_ptr<TypeNode> &type)
      : Stmt(loc), name_(name), type_(std::move(type)) {
    CHECK_PTR(type_);
  }
  VarDecl(const SourceLocation loc, const std::string &name, TypeNode *type)
      : Stmt(loc), name_(name), type_(type) {
    CHECK_PTR(type_);
  }
  VarDecl(const SourceLocation loc, const std::string &name,
          std::unique_ptr<TypeNode> &type, std::unique_ptr<Expr> &init)
      : Stmt(loc), name_(name), type_(std::move(type)), init_(std::move(init)) {
    CHECK_PTR(type_);
  }
  VarDecl(const SourceLocation loc, const std::string &name, TypeNode *type,
          std::unique_ptr<Expr> &init)
      : Stmt(loc), name_(name), type_(type), init_(std::move(init)) {
    CHECK_PTR(type_);
  }
  NodeKind getKind() const override { return NODE_VARDECL; }

  const std::string &getName() const { return name_; }
  const TypeNode &getTypeNode() const { return *type_; }
  std::unique_ptr<Type> getType() const { return type_->toType(); }
  bool hasInit() const { return bool(init_); }
  const Expr &getInit() const {
    CHECK(hasInit(), "This VarDecl was created without an initial value.");
    return *init_;
  }

 private:
  std::string name_;
  std::unique_ptr<TypeNode> type_;
  std::unique_ptr<Expr> init_;
};

class TypeDef : public Stmt {
 public:
  TypeDef(const SourceLocation loc, const std::string &name,
          const std::vector<std::unique_ptr<VarDecl>> &members)
    : Stmt(loc), name_(name), members_(std::move(members)) {
      CHECK_PTRS(members_);
    }

  NodeKind getKind() const override { return NODE_TYPEDEF; }
  const std::string &getName() const { return name_; }
  const std::vector<std::unique_ptr<VarDecl>> &getMembers() const {
    return members_;
  }

 private:
  std::string name_;
  std::vector<std::unique_ptr<VarDecl>> members_;
};

class Param : public Node {
 public:
  Param(const SourceLocation loc, const std::string &name,
        std::unique_ptr<TypeNode> &type)
      : Node(loc), name_(name), type_(std::move(type)) {
    CHECK_PTR(type_);
  }
  NodeKind getKind() const override { return NODE_PARAM; }

  const std::string &getName() const { return name_; }
  const TypeNode &getTypeNode() const { return *type_; }

 private:
  std::string name_;
  std::unique_ptr<TypeNode> type_;
};

/**
 * (argc: i32, argv: i8**) -> i32
 */
class FuncTypeNode : public TypeNode {
 public:
  FuncTypeNode(const SourceLocation loc, std::unique_ptr<TypeNode> &ret_type,
               std::vector<std::unique_ptr<Param>> &params, bool isvararg)
      : TypeNode(loc),
        ret_type_(std::move(ret_type)),
        params_(std::move(params)),
        isvararg_(isvararg) {
    CHECK_PTR(ret_type_);
    CHECK_PTRS(params_);
  }

  const TypeNode &getReturnTypeNode() const { return *ret_type_; }
  const std::vector<std::unique_ptr<Param>> &getParams() const {
    return params_;
  }
  bool isVarArg() const { return isvararg_; }

  NodeKind getKind() const override { return NODE_FUNC_TYPE; }
  std::unique_ptr<Type> toType() const override {
    auto ret_type = ret_type_->toType();
    std::vector<std::unique_ptr<Type>> arg_types;
    for (const auto &param_ptr : params_) {
      auto param_type = param_ptr->getTypeNode().toType();
      arg_types.push_back(std::move(param_type));
    }
    return std::unique_ptr<Type>(new FuncType(ret_type, arg_types, isvararg_));
  }

 private:
  std::unique_ptr<TypeNode> ret_type_;
  std::vector<std::unique_ptr<Param>> params_;
  bool isvararg_;
};

/**
 * <ID> : <FuncTypeNode>
 */
class FuncDecl : public VarDecl {
 public:
  FuncDecl(const SourceLocation loc, const std::string &name,
           std::unique_ptr<FuncTypeNode> &func_type)
      : VarDecl(loc, name, func_type.release()) {}

  NodeKind getKind() const override { return NODE_FUNCDECL; }
};

/**
 * <ID> = <Expr>;
 */
class Assign : public Stmt {
 public:
  Assign(std::unique_ptr<Expr> &lhs, std::unique_ptr<Expr> &expr)
      : Stmt(lhs->getLoc()), lhs_(std::move(lhs)), expr_(std::move(expr)) {
    CHECK_PTR(lhs_);
    CHECK_PTR(expr_);
  }

  NodeKind getKind() const override { return NODE_ASSIGN; }
  const Expr &getLHS() const { return *lhs_; }
  const Expr &getExpr() const { return *expr_; }

 private:
  std::unique_ptr<Expr> lhs_;
  std::unique_ptr<Expr> expr_;
};

class CallStmt : public Stmt {
 public:
  CallStmt(std::unique_ptr<Call> &call)
      : Stmt(call->getLoc()), call_(std::move(call)) {
    CHECK_PTR(call_);
  }

  NodeKind getKind() const override { return NODE_CALLSTMT; }
  const Call &getCall() const { return *call_; }

 private:
  std::unique_ptr<Call> call_;
};

class If : public Stmt {
 public:
  If(const SourceLocation loc, std::unique_ptr<Expr> &cond,
     std::vector<std::unique_ptr<Stmt>> &body)
      : Stmt(loc), cond_(std::move(cond)), body_(std::move(body)) {
    CHECK_PTR(cond_);
    CHECK_PTRS(body_);
  }
  If(const SourceLocation loc, std::unique_ptr<Expr> &cond,
     std::vector<std::unique_ptr<Stmt>> &body,
     std::vector<std::unique_ptr<Stmt>> &else_body)
      : Stmt(loc),
        cond_(std::move(cond)),
        body_(std::move(body)),
        else_body_(std::move(else_body)) {
    CHECK_PTR(cond_);
    CHECK_PTRS(body_);
    CHECK_PTRS(else_body_);
  }
  NodeKind getKind() const override { return NODE_IF; }

  const Expr &getCond() const { return *cond_; }
  const std::vector<std::unique_ptr<Stmt>> &getBody() const { return body_; }
  const std::vector<std::unique_ptr<Stmt>> &getElseBody() const {
    return else_body_;
  }
  bool hasElse() const { return else_body_.empty(); }

 private:
  std::unique_ptr<Expr> cond_;
  std::vector<std::unique_ptr<Stmt>> body_;
  std::vector<std::unique_ptr<Stmt>> else_body_;
};

class While : public Stmt {
 public:
  While(const SourceLocation loc, std::unique_ptr<Expr> &cond,
        std::vector<std::unique_ptr<Stmt>> &body)
      : Stmt(loc), cond_(std::move(cond)), body_(std::move(body)) {
    CHECK_PTR(cond_);
    CHECK_PTRS(body_);
  }

  NodeKind getKind() const override { return NODE_WHILE; }

  const Expr &getCond() const { return *cond_; }
  const std::vector<std::unique_ptr<Stmt>> &getBody() const { return body_; }

 private:
  std::unique_ptr<Expr> cond_;
  std::vector<std::unique_ptr<Stmt>> body_;
};

class ExternVarDecl : public ExternDecl {
 public:
  ExternVarDecl(std::unique_ptr<VarDecl> &decl)
      : ExternDecl(decl->getLoc(), decl->getName()), decl_(std::move(decl)) {
    CHECK_PTR(decl_);
  }
  ExternVarDecl(VarDecl *decl)
      : ExternDecl(decl->getLoc(), decl->getName()), decl_(decl) {
    CHECK_PTR(decl_);
  }

  NodeKind getKind() const override { return NODE_EXTERN_VARDECL; }

  const VarDecl &getDecl() const { return *decl_; }

 private:
  std::unique_ptr<VarDecl> decl_;
};

class ExternTypeDef : public ExternDecl {
 public:
  ExternTypeDef(std::unique_ptr<TypeDef> &type_def)
    : ExternDecl(type_def->getLoc(), type_def->getName()),
      type_def_(std::move(type_def)) {
    CHECK_PTR(type_def_);
  }

  NodeKind getKind() const override { return NODE_EXTERN_FUNCDEF; }
  const TypeDef &getTypeDef() const { return *type_def_; }

 private:
  std::unique_ptr<TypeDef> type_def_;
};

class FuncDef : public ExternDecl {
 public:
  FuncDef(std::unique_ptr<FuncDecl> &decl,
          std::vector<std::unique_ptr<Stmt>> &stmts)
      : ExternDecl(decl->getLoc(), decl->getName()),
        decl_(std::move(decl)),
        stmts_(std::move(stmts)) {
    CHECK_PTR(decl_);
    CHECK_PTRS(stmts_);
  }
  NodeKind getKind() const override { return NODE_FUNCDEF; }
  const FuncDecl &getDecl() const { return *decl_; }
  const std::vector<std::unique_ptr<Stmt>> &getStmts() const { return stmts_; }

 private:
  std::unique_ptr<FuncDecl> decl_;
  std::vector<std::unique_ptr<Stmt>> stmts_;
};

class Return : public Stmt {
 public:
  Return(const SourceLocation loc, std::unique_ptr<Expr> &expr)
      : Stmt(loc), expr_(std::move(expr)) {
    CHECK_PTR(expr_);
  }
  NodeKind getKind() const override { return NODE_RET; }

  const Expr &getExpr() const { return *expr_; }

 private:
  std::unique_ptr<Expr> expr_;
};

class Parser {
 public:
  Parser(Lexer &lexer) : lexer_(lexer) {}
  Parser(Lexer &lexer, const Diagnostic &diag) : lexer_(lexer), diag_(diag) {}

  bool ParseModule(std::unique_ptr<Module> &result);

 private:
  bool ParseExternDecl(std::unique_ptr<ExternDecl> &result);
  bool ParseExternTypeDef(std::unique_ptr<ExternTypeDef> &result);
  bool ParseBracedStmts(std::vector<std::unique_ptr<Stmt>> &stmts);
  bool ParseStmt(std::unique_ptr<Stmt> &stmt);
  bool ParseReturn(std::unique_ptr<Return> &result);
  bool ParseIf(std::unique_ptr<If> &result);
  bool ParseWhile(std::unique_ptr<While> &result);
  bool ParseExpr(std::unique_ptr<Expr> &expr);
  bool ParseSingleExpr(std::unique_ptr<Expr> &result);
  bool ParseTypeNode(std::unique_ptr<TypeNode> &result);
  bool ParseFuncTypeNode(std::unique_ptr<FuncTypeNode> &result);
  bool ParseParam(std::unique_ptr<Param> &result);
  bool ParseTypeDef(std::unique_ptr<TypeDef> &result);

  // Parse nodes, but already having lexed one ID token from the lexer.
  bool ParseVarDeclAfterID(const Token &id_tok,
                           std::unique_ptr<VarDecl> &result);
  bool ParseExprAfterID(const Token &id_tok, std::unique_ptr<Expr> &result);
  bool ParseSingleExprAfterID(const Token &id_tok,
                              std::unique_ptr<Expr> &result);

  // Attempt to parse a call given an expression. If we are able to make a call
  // but run into an error parsing it, we return false. Otherwise, return true.
  bool TryToMakeCallAfterExpr(std::unique_ptr<Expr> &expr);
  bool TryToParseCompoundExpr(std::unique_ptr<Expr> &expr);

  const Diagnostic &getDiag() const { return diag_; }

  Lexer &lexer_;
  const Diagnostic diag_;
};

}  // namespace qwip

#endif
