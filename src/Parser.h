#ifndef PARSER_H
#define PARSER_H

#include <memory>
#include <unordered_map>
#include <vector>

#include "Diagnostics.h"
#include "Lexer.h"

namespace qwip {

enum NodeKind {
#define NODE(Kind, Class) Kind,
#include "Nodes.def"
};

#define assert_ptr(PTR) assert(bool(PTR) && "Expected a non-null pointer.")

template <typename T>
void __CheckUptrVector(const std::vector<std::unique_ptr<T>> &ptrs) {
  for (const auto &ptr : ptrs) assert_ptr(ptr);
}
#define assert_ptr_vector(PTRS) __CheckUptrVector(PTRS)

enum TypeKind {
#define TYPE(Kind, Class) Kind,
#include "Types.def"
};

std::string TypeKindToString(TypeKind kind);
std::string NodeKindToString(NodeKind kind);

class Type {
 public:
  virtual ~Type() {}
  virtual TypeKind getKind() const = 0;
  virtual std::unique_ptr<Type> Clone() const = 0;

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
    assert_ptr(ret_type_);
    assert_ptr_vector(arg_types_);
  }

  TypeKind getKind() const override { return TYPE_FUNC; }
  const Type &getReturnType() const { return *ret_type_; }
  const std::vector<std::unique_ptr<Type>> &getArgTypes() const {
    return arg_types_;
  }
  bool isVarArg() const { return isvararg_; }
  std::unique_ptr<Type> Clone() const override {
    std::vector<std::unique_ptr<Type>> args;
    for (const auto &arg_ptr : arg_types_) {
      args.push_back(arg_ptr->Clone());
    }
    auto ret_type = ret_type_->Clone();
    return std::unique_ptr<Type>(new FuncType(ret_type, args, isvararg_));
  }

 private:
  std::unique_ptr<Type> ret_type_;
  std::vector<std::unique_ptr<Type>> arg_types_;
  bool isvararg_;
};

class StructType : public Type {
 public:
  StructType() {}
  StructType(std::vector<std::unique_ptr<Type>> &types,
             const std::unordered_map<std::string, unsigned> &idxs)
      : types_(std::move(types)), idxs_(idxs) {}

  TypeKind getKind() const override { return TYPE_STRUCT; }
  std::unique_ptr<Type> Clone() const override {
    std::vector<std::unique_ptr<Type>> types;
    for (const auto &type_ptr : types_) {
      types.push_back(type_ptr->Clone());
    }
    return std::unique_ptr<StructType>(new StructType(types, idxs_));
  }

  void addMember(const std::string &name, std::unique_ptr<Type> &type) {
    idxs_[name] = types_.size();
    types_.push_back(std::move(type));
  }
  const Type &getMember(unsigned i) const { return *(types_[i]); }
  const Type &getMember(const std::string &name) const {
    return getMember(idxs_.at(name));
  }
  unsigned getIndex(const std::string &name) const { return idxs_.at(name); }
  const std::vector<std::unique_ptr<Type>> &getTypes() const { return types_; }

 private:
  std::vector<std::unique_ptr<Type>> types_;
  std::unordered_map<std::string, unsigned> idxs_;
};

class StrType : public Type {
 public:
  StrType(unsigned size) : size_(size) {}
  TypeKind getKind() const override { return TYPE_STR; }
  unsigned getSize() const { return size_; }
  std::unique_ptr<Type> Clone() const override {
    return std::unique_ptr<Type>(new StrType(size_));
  }

 private:
  unsigned size_;
};

class IntType : public Type {
 public:
  IntType(unsigned numbits) : numbits_(numbits) {}
  TypeKind getKind() const override { return TYPE_INT; }
  unsigned getNumBits() const { return numbits_; }
  std::unique_ptr<Type> Clone() const override {
    return std::unique_ptr<Type>(new IntType(numbits_));
  }

 private:
  unsigned numbits_;
};

class IDType : public Type {
 public:
  IDType(const std::string &name) : name_(name) {}

  TypeKind getKind() const override { return TYPE_ID; }
  const std::string &getName() const { return name_; }
  std::unique_ptr<Type> Clone() const override {
    return std::unique_ptr<Type>(new IDType(name_));
  }

 private:
  std::string name_;
};

class PtrType : public Type {
 public:
  PtrType(std::unique_ptr<Type> &pointee) : pointee_(std::move(pointee)) {
    assert_ptr(pointee_);
  }

  TypeKind getKind() const override { return TYPE_PTR; }
  const Type &getPointeeType() const { return *pointee_; }
  std::unique_ptr<Type> Clone() const override {
    auto pointee = pointee_->Clone();
    return std::unique_ptr<Type>(new PtrType(pointee));
  }

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
  ExternDecl(const SourceLocation loc) : Node(loc) {}
};

class Module : public Node {
 public:
  Module(const SourceLocation &loc,
         std::vector<std::unique_ptr<ExternDecl>> &decls)
      : Node(loc), decls_(std::move(decls)) {
    assert_ptr_vector(decls_);
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
  virtual std::unique_ptr<Type> getType() const = 0;
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

  std::unique_ptr<Type> getType() const override {
    std::unique_ptr<Type> caller_type = caller_->getType();
    assert(caller_type->getKind() == TYPE_FUNC &&
           "Expected the caller to be a function type.");
    return caller_type->getAs<FuncType>().getReturnType().Clone();
  }

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
  std::unique_ptr<Type> getType() const override {
    return std::make_unique<StrType>(val_.size() + 1);
  }

 private:
  std::string val_;
};

class Int : public Expr {
 public:
  Int(const SourceLocation loc, int i) : Expr(loc), val_(i) {}
  NodeKind getKind() const override { return NODE_INT; }
  std::unique_ptr<Type> getType() const override {
    return std::make_unique<IntType>(sizeof(val_));
  }

  int getVal() const { return val_; }

 private:
  int val_;
};

class ID : public Expr {
 public:
  ID(const SourceLocation loc, const std::string &name,
     std::unique_ptr<Type> &type)
      : Expr(loc), name_(name), type_(std::move(type)) {
    assert_ptr(type_);
  }
  ID(const SourceLocation loc, const std::string &name, const Type &type)
      : Expr(loc), name_(name), type_(type.Clone()) {
    assert_ptr(type_);
  }
  NodeKind getKind() const override { return NODE_ID; }
  std::unique_ptr<Type> getType() const override { return type_->Clone(); }

  const std::string &getName() const { return name_; }
  const Type &getTypeRef() const { return *type_; }

 private:
  std::string name_;
  std::unique_ptr<Type> type_;
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
    assert_ptr(lhs_);
    assert_ptr(rhs_);
  }

  NodeKind getKind() const override { return NODE_BINOP; }
  const Expr &getLHS() const { return *lhs_; }
  const Expr &getRHS() const { return *rhs_; }
  BinOpCode getBinOp() const { return op_; }
  std::unique_ptr<Type> getType() const override {
    switch (op_) {
      case BINOP_LE:
      case BINOP_LT:
        return std::make_unique<IntType>(/*numbits=*/32);
      case BINOP_ADD:
      case BINOP_SUB:
        // TODO: The result type should depend on the types of both operands.
        return lhs_->getType();
    }
    UNREACHABLE("Unhandled BinOpCode");
    return nullptr;
  }

 private:
  std::unique_ptr<Expr> lhs_;
  std::unique_ptr<Expr> rhs_;
  BinOpCode op_;
};

class MemberAccess : public Expr {
 public:
  MemberAccess(std::unique_ptr<Expr> &base, const std::string &member)
      : Expr(base->getLoc()), base_(std::move(base)), member_(member) {
    assert_ptr(base_);
    assert(base_->getType()->getKind() == TYPE_STRUCT &&
           "The base should be a struct type.");
  }

  NodeKind getKind() const override { return NODE_MEMBER_ACCESS; }
  const Expr &getBase() const { return *base_; }
  const std::string &getMember() const { return member_; }
  std::unique_ptr<Type> getType() const override {
    std::unique_ptr<Type> base_type = base_->getType();
    return base_type->getAs<StructType>().getMember(member_).Clone();
  }

 private:
  std::unique_ptr<Expr> base_;
  std::string member_;
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
    assert_ptr(pointee_type_);
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
    assert_ptr(type_);
  }
  VarDecl(const SourceLocation loc, const std::string &name, TypeNode *type)
      : Stmt(loc), name_(name), type_(type) {
    assert_ptr(type_);
  }
  NodeKind getKind() const override { return NODE_VARDECL; }

  const std::string &getName() const { return name_; }
  const TypeNode &getTypeNode() const { return *type_; }
  std::unique_ptr<Type> getType() const { return type_->toType(); }

 private:
  std::string name_;
  std::unique_ptr<TypeNode> type_;
};

class VarDef : public Stmt {
 public:
  VarDef(std::unique_ptr<VarDecl> &decl, std::unique_ptr<Expr> &init)
      : Stmt(decl->getLoc()), decl_(std::move(decl)), init_(std::move(init)) {
    assert_ptr(decl_);
    assert_ptr(init_);
  }

  NodeKind getKind() const override { return NODE_VARDEF; }

  const VarDecl &getDecl() const { return *decl_; }
  const Expr &getInit() const { return *init_; }

 private:
  std::unique_ptr<VarDecl> decl_;
  std::unique_ptr<Expr> init_;
};

class MemberDecl : public Node {
 public:
  MemberDecl(const SourceLocation loc) : Node(loc) {}
};

class TypeDef : public Stmt {
 public:
  TypeDef(const SourceLocation loc, const std::string &name,
          std::vector<std::unique_ptr<MemberDecl>> &members)
      : Stmt(loc), name_(name), members_(std::move(members)) {
    assert_ptr_vector(members_);
  }

  NodeKind getKind() const override { return NODE_TYPEDEF; }
  const std::string &getName() const { return name_; }
  const std::vector<std::unique_ptr<MemberDecl>> &getMembers() const {
    return members_;
  }

  StructType getStructType() const;

 private:
  std::string name_;
  std::vector<std::unique_ptr<MemberDecl>> members_;
};

class Param : public Node {
 public:
  Param(const SourceLocation loc, const std::string &name,
        std::unique_ptr<TypeNode> &type)
      : Node(loc), name_(name), type_(std::move(type)) {
    assert_ptr(type_);
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
    assert_ptr(ret_type_);
    assert_ptr_vector(params_);
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
 * <ID> = <Expr>;
 */
class Assign : public Stmt {
 public:
  Assign(std::unique_ptr<Expr> &lhs, std::unique_ptr<Expr> &expr)
      : Stmt(lhs->getLoc()), lhs_(std::move(lhs)), expr_(std::move(expr)) {
    assert_ptr(lhs_);
    assert_ptr(expr_);
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
    assert_ptr(call_);
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
    assert_ptr(cond_);
    assert_ptr_vector(body_);
  }
  If(const SourceLocation loc, std::unique_ptr<Expr> &cond,
     std::vector<std::unique_ptr<Stmt>> &body,
     std::vector<std::unique_ptr<Stmt>> &else_body)
      : Stmt(loc),
        cond_(std::move(cond)),
        body_(std::move(body)),
        else_body_(std::move(else_body)) {
    assert_ptr(cond_);
    assert_ptr_vector(body_);
    assert_ptr_vector(else_body_);
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
    assert_ptr(cond_);
    assert_ptr_vector(body_);
  }

  NodeKind getKind() const override { return NODE_WHILE; }

  const Expr &getCond() const { return *cond_; }
  const std::vector<std::unique_ptr<Stmt>> &getBody() const { return body_; }

 private:
  std::unique_ptr<Expr> cond_;
  std::vector<std::unique_ptr<Stmt>> body_;
};

class FuncDef : public Stmt {
 public:
  FuncDef(std::unique_ptr<VarDecl> &decl,
          std::vector<std::unique_ptr<Stmt>> &stmts)
      : Stmt(decl->getLoc()), decl_(std::move(decl)), stmts_(std::move(stmts)) {
    assert_ptr(decl_);
    assert_ptr_vector(stmts_);
    assert(decl_->getTypeNode().getKind() == NODE_FUNC_TYPE &&
           "A FuncDef node can only accept a VarDecl that is a function type.");
  }
  NodeKind getKind() const override { return NODE_FUNCDEF; }
  const VarDecl &getDecl() const { return *decl_; }
  const std::vector<std::unique_ptr<Stmt>> &getStmts() const { return stmts_; }

 private:
  std::unique_ptr<VarDecl> decl_;
  std::vector<std::unique_ptr<Stmt>> stmts_;
};

class Return : public Stmt {
 public:
  Return(const SourceLocation loc, std::unique_ptr<Expr> &expr)
      : Stmt(loc), expr_(std::move(expr)) {
    assert_ptr(expr_);
  }
  NodeKind getKind() const override { return NODE_RET; }

  const Expr &getExpr() const { return *expr_; }

 private:
  std::unique_ptr<Expr> expr_;
};

// A special convenience macro for defining a node class that's just meant to
// hold an instance of another class.
#define DEFINE_WRAPPER_NODE(Class, Kind, Parent, InnerClass)      \
  class Class : public Parent {                                   \
   public:                                                        \
    Class(std::unique_ptr<InnerClass> &inner)                     \
        : Parent(inner->getLoc()), inner_(std::move(inner)) {     \
      assert_ptr(inner_);                                         \
    }                                                             \
    NodeKind getKind() const override { return Kind; }            \
    const InnerClass &get##InnerClass() const { return *inner_; } \
                                                                  \
   private:                                                       \
    std::unique_ptr<InnerClass> inner_;                           \
  };

DEFINE_WRAPPER_NODE(ExternVarDecl, NODE_EXTERN_VARDECL, ExternDecl, VarDecl)
DEFINE_WRAPPER_NODE(ExternVarDef, NODE_EXTERN_VARDEF, ExternDecl, VarDef)
DEFINE_WRAPPER_NODE(ExternTypeDef, NODE_EXTERN_TYPEDEF, ExternDecl, TypeDef)
DEFINE_WRAPPER_NODE(ExternFuncDef, NODE_EXTERN_FUNCDEF, ExternDecl, FuncDef)
DEFINE_WRAPPER_NODE(MemberVarDecl, NODE_MEMBER_VARDECL, MemberDecl, VarDecl)

typedef std::unordered_map<std::string, std::unique_ptr<Type>> TypeMap;
typedef std::unordered_map<std::string, std::unique_ptr<Type>> VarMap;

class Context {
 public:
  Context() {}

  const TypeMap &getTypes() const { return types_; }
  void addType(const std::string &type_name, const Type &type) {
    types_[type_name] = type.Clone();
  }
  const Type *getType(const std::string &type_name) const {
    auto foundtype = types_.find(type_name);
    if (foundtype != types_.end()) return foundtype->second.get();
    return nullptr;
  }

  const TypeMap &getVars() const { return vars_; }
  void addVar(const std::string &varname, const Type &type) {
    vars_[varname] = type.Clone();
  }
  const Type *getVarType(const std::string &varname) const {
    auto foundtype = vars_.find(varname);
    if (foundtype != vars_.end()) return foundtype->second.get();
    return nullptr;
  }

 private:
  TypeMap types_;
  VarMap vars_;
};

class Parser {
 public:
  Parser(Lexer &lexer) : lexer_(lexer) {}
  Parser(Lexer &lexer, const Diagnostic &diag) : lexer_(lexer), diag_(diag) {}

  bool Parse(std::unique_ptr<Module> &result);

 private:
  bool ParseBracedStmts(std::vector<std::unique_ptr<Stmt>> &stmts);
  bool EnterScopeAndParseBracedStmts(std::vector<std::unique_ptr<Stmt>> &stmts);
  bool EnterScopeAndParseFuncBracedStmts(
      const FuncTypeNode &functype, std::vector<std::unique_ptr<Stmt>> &stmts);
  bool ParseTypeNode(std::unique_ptr<TypeNode> &result);
  bool ParseSingleExpr(std::unique_ptr<Expr> &result);
  bool ParseStmt(std::unique_ptr<Stmt> &result);
  bool ParseExpr(std::unique_ptr<Expr> &result);

#define NODE(Kind, Class) bool Parse##Class(std::unique_ptr<Class> &node);
#include "Nodes.def"

  bool ParseExprAfterID(const Token &id_tok, std::unique_ptr<Expr> &result);
  bool ParseSingleExprAfterID(const Token &id_tok,
                              std::unique_ptr<Expr> &result);
  bool ParseNamedDeclOrDefAfterID(const Token &id_tok,
                                  std::unique_ptr<Stmt> &result);
  bool ParseNamedDeclOrDef(std::unique_ptr<Stmt> &result);
  bool ParseNamedExternDeclOrDef(std::unique_ptr<ExternDecl> &result);

  // Attempt to parse a call given an expression. If we are able to make a call
  // but run into an error parsing it, we return false. Otherwise, return true.
  bool TryToMakeCallAfterExpr(std::unique_ptr<Expr> &expr);
  bool TryToParseCompoundExpr(std::unique_ptr<Expr> &expr);

  Context &getContext() {
    assert(!contexts_.empty() &&
           "We have not parsed a module yet if there are no contexts on the "
           "stack.");
    return contexts_.back();
  }
  const Type *getTypeForVar(const std::string &name) const {
    for (unsigned i = 0; i < contexts_.size(); ++i) {
      if (const Type *type =
              contexts_.at(contexts_.size() - i - 1).getVarType(name)) {
        // Resolve the type node if it is a typedef.
        if (type->getKind() == TYPE_ID) {
          if (const Type *canon_type = getType(type->getAs<IDType>().getName()))
            return canon_type;
        }
        return type;
      }
    }
    return nullptr;
  }
  const Type *getType(const std::string &name) const {
    for (unsigned i = 0; i < contexts_.size(); ++i) {
      if (const Type *type =
              contexts_.at(contexts_.size() - i - 1).getType(name))
        return type;
    }
    return nullptr;
  }
  void EnterScope() { contexts_.emplace_back(); }
  void ExitScope() { contexts_.pop_back(); }
  const Diagnostic &getDiag() const { return diag_; }

  Lexer &lexer_;
  const Diagnostic diag_;
  std::vector<Context> contexts_;
};

}  // namespace qwip

#endif
