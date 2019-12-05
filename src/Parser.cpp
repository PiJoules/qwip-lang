#include "Parser.h"

#include <cassert>

// Convenience macros for lexing and exiting a method on a lex error.
#define TRY_LEX(Lexer, Tok) \
  if (!Lexer.Lex(Tok)) return false;

#define TRY_PEEK(Lexer, Tok) \
  if (!Lexer.Peek(Tok)) return false;

namespace qwip {

static std::string TypeKindToString(TypeKind kind) {
  switch (kind) {
#define TYPE(Kind, Class) \
  case Kind:              \
    return STR(Kind);
#include "Types.def"
  }
  UNREACHABLE("Unknown type");
  return "";
}

std::string NodeKindToString(NodeKind kind) {
  switch (kind) {
#define NODE(Kind, Class) \
  case Kind:              \
    return STR(Kind);
#include "Nodes.def"
  }
  UNREACHABLE("Unknown node");
  return "";
}

bool Parser::Parse(std::unique_ptr<Module> &result) {
  return ParseModule(result);
}

bool Parser::ParseModule(std::unique_ptr<Module> &result) {
  Token lookahead;
  TRY_PEEK(lexer_, lookahead);

  // Initialize this here only.
  global_context_ = std::make_unique<Context>();
  current_context_ = global_context_.get();

  std::vector<std::unique_ptr<ExternDecl>> decls;
  while (lookahead.kind != TOK_EOF) {
    if (lookahead.kind == TOK_ID) {
      std::unique_ptr<ExternDecl> externdecl;
      if (!ParseNamedExternDeclOrDef(externdecl)) return false;
      decls.push_back(std::move(externdecl));
    } else if (lookahead.kind == TOK_TYPE) {
      std::unique_ptr<ExternTypeDef> externtypedef;
      if (!ParseExternTypeDef(externtypedef)) return false;
      decls.push_back(std::move(externtypedef));
    } else if (lookahead.kind == TOK_ENUM) {
      std::unique_ptr<ExternEnumDef> externenumdef;
      if (!ParseExternEnumDef(externenumdef)) return false;
      decls.push_back(std::move(externenumdef));
    } else {
      diag_.Err(lookahead.loc)
          << "Unexpected token found. External declarations in a module should "
             "start with an ID.";
      return false;
    }

    TRY_PEEK(lexer_, lookahead);
  }

  SourceLocation loc;
  loc.line = 1;
  loc.col = 1;
  loc.filename = lexer_.getFilename();
  result = std::make_unique<Module>(loc, decls);
  return true;
}

bool Parser::ParseNamedExternDeclOrDef(std::unique_ptr<ExternDecl> &result) {
  std::unique_ptr<Stmt> func;
  if (!ParseNamedDeclOrDef(func)) return false;

  if (func->getKind() == NODE_FUNCDEF) {
    std::unique_ptr<FuncDef> funcdef(static_cast<FuncDef *>(func.release()));
    result = std::make_unique<ExternFuncDef>(funcdef);
  } else if (func->getKind() == NODE_VARDEF) {
    std::unique_ptr<VarDef> vardef(static_cast<VarDef *>(func.release()));
    result = std::make_unique<ExternVarDef>(vardef);
  } else {
    assert(func->getKind() == NODE_VARDECL && "Unhandled decl or def kind");
    std::unique_ptr<VarDecl> vardecl(static_cast<VarDecl *>(func.release()));
    result = std::make_unique<ExternVarDecl>(vardecl);
  }
  return true;
}

bool Parser::ParseExternEnumDef(std::unique_ptr<ExternEnumDef> &result) {
  Token tok;
  TRY_LEX(lexer_, tok);  // enum
  assert(tok.kind == TOK_ENUM &&
         "Only call ParseExternEnumDef if the previous token was a 'enum'.");
  SourceLocation enumloc = tok.loc;

  // <id>
  TRY_LEX(lexer_, tok);
  if (tok.kind != TOK_ID) {
    diag_.Err(tok.loc) << "Expected the name of the custom enum.";
    return false;
  }
  std::string name = tok.chars;

  // {
  TRY_LEX(lexer_, tok);
  if (tok.kind != TOK_LBRACE) {
    diag_.Err(tok.loc) << "Expected an opening '{' in the custom enum.";
    return false;
  }

  // Enum values
  std::vector<std::string> values;
  while (1) {
    Token lookahead;
    TRY_PEEK(lexer_, lookahead);

    if (lookahead.kind == TOK_RBRACE) break;

    TRY_PEEK(lexer_, tok);
    if (tok.kind != TOK_ID) {
      diag_.Err(tok.loc) << "Expected a name for a value in this enum.";
      return false;
    }
    TRY_LEX(lexer_, tok);
    values.push_back(tok.chars);

    // Consume a ','
    TRY_PEEK(lexer_, tok);
    if (tok.kind == TOK_COMMA) {
      TRY_LEX(lexer_, tok);
    } else if (tok.kind == TOK_RBRACE) {
      break;
    } else {
      diag_.Err(tok.loc) << "Expected a ',' or the end brace '}' to indicate "
                            "the end of an enum definition.";
      return false;
    }
  }

  // }
  TRY_LEX(lexer_, tok);
  assert(tok.kind == TOK_RBRACE &&
         "Should only break out of the previous loop if we ran into a closing "
         "brace.");

  auto enum_def = std::make_unique<EnumDef>(enumloc, name, values);
  EnumType type = enum_def->getEnumType();
  getContext().addType(name, type);

  for (unsigned i = 0; i < values.size(); ++i) {
    getContext().addEnumLiteral(values[i], type);
  }

  result = std::make_unique<ExternEnumDef>(enum_def);
  return true;
}

bool Parser::ParseExternTypeDef(std::unique_ptr<ExternTypeDef> &result) {
  Token tok;
  TRY_LEX(lexer_, tok);  // type
  assert(tok.kind == TOK_TYPE &&
         "Only call ParseExternTypeDef if the previous token was a 'type'.");
  SourceLocation typeloc = tok.loc;

  // <id>
  TRY_LEX(lexer_, tok);
  if (tok.kind != TOK_ID) {
    diag_.Err(tok.loc) << "Expected the name of the custom type.";
    return false;
  }
  std::string name = tok.chars;

  // :
  TRY_LEX(lexer_, tok);
  if (tok.kind != TOK_COL) {
    diag_.Err(tok.loc) << "Expected ':' after the name of the custom type.";
    return false;
  }

  // {
  TRY_LEX(lexer_, tok);
  if (tok.kind != TOK_LBRACE) {
    diag_.Err(tok.loc) << "Expected an opening '{' in the custom type.";
    return false;
  }

  EnterScope();

  // Member declarations
  std::vector<std::unique_ptr<MemberDecl>> members;
  while (1) {
    Token lookahead;
    TRY_PEEK(lexer_, lookahead);

    if (lookahead.kind == TOK_RBRACE) break;

    TRY_PEEK(lexer_, tok);
    if (tok.kind != TOK_ID) {
      diag_.Err(tok.loc) << "Expected a name for a member in the type.";
      return false;
    }

    // Consume and read statements.
    std::unique_ptr<Stmt> nameddecl;
    if (!ParseNamedDeclOrDef(nameddecl)) return false;
    if (nameddecl->getKind() == NODE_VARDECL) {
      std::unique_ptr<VarDecl> vardecl(
          static_cast<VarDecl *>(nameddecl.release()));
      members.push_back(std::make_unique<MemberVarDecl>(vardecl));
    } else {
      diag_.Err(nameddecl->getLoc())
          << "A new struct can only contain declarations.";
      return false;
    }
  }

  ExitScope();

  // }
  TRY_LEX(lexer_, tok);
  assert(tok.kind == TOK_RBRACE &&
         "Should only break out of the previous loop if we ran into a closing "
         "brace.");

  auto type_def = std::make_unique<TypeDef>(typeloc, name, members);
  StructType type = type_def->getStructType();
  getContext().addType(name, type);
  result = std::make_unique<ExternTypeDef>(type_def);
  return true;
}

bool Parser::EnterScopeAndParseBracedStmts(
    std::vector<std::unique_ptr<Stmt>> &stmts) {
  EnterScope();
  bool success = ParseBracedStmts(stmts);
  ExitScope();
  return success;
}

bool Parser::EnterScopeAndParseFuncBracedStmts(
    const FuncTypeNode &functype, std::vector<std::unique_ptr<Stmt>> &stmts) {
  EnterScope();
  for (const auto &param_ptr : functype.getParams()) {
    auto type = param_ptr->getTypeNode().toType();
    getContext().addVar(param_ptr->getName(), *type);
  }
  bool success = ParseBracedStmts(stmts);
  ExitScope();
  return success;
}

bool Parser::ParseBracedStmts(std::vector<std::unique_ptr<Stmt>> &stmts) {
  // {
  Token tok;
  TRY_LEX(lexer_, tok);
  assert(tok.kind == TOK_LBRACE &&
         "Do not call ParseBracedStmts unless the previous lookahead was an "
         "opening brace.");

  // Statements
  while (1) {
    Token lookahead;
    TRY_PEEK(lexer_, lookahead);

    if (lookahead.kind == TOK_RBRACE) break;

    // Consume and read statements.
    std::unique_ptr<Stmt> stmt;
    if (!ParseStmt(stmt)) {
      return false;
    }
    stmts.push_back(std::move(stmt));
  }

  // }
  TRY_LEX(lexer_, tok);
  assert(tok.kind == TOK_RBRACE &&
         "Should only break out of the previous loop if we ran into a closing "
         "brace.");

  return true;
}

bool Parser::ParseStmt(std::unique_ptr<Stmt> &result) {
  Token lookahead, tok;
  TRY_PEEK(lexer_, lookahead);
  SourceLocation stmtloc = lookahead.loc;

  if (lookahead.kind == TOK_RET) {
    std::unique_ptr<Return> stmt;
    if (!ParseReturn(stmt)) return false;
    result = std::move(stmt);
    return true;
  } else if (lookahead.kind == TOK_IF) {
    std::unique_ptr<If> stmt;
    if (!ParseIf(stmt)) return false;
    result = std::move(stmt);
    return true;
  } else if (lookahead.kind == TOK_WHILE) {
    std::unique_ptr<While> stmt;
    if (!ParseWhile(stmt)) return false;
    result = std::move(stmt);
    return true;
  } else if (lookahead.kind == TOK_ID) {
    // Parse an expression statement or a variable declaration.
    Token id_tok;
    TRY_LEX(lexer_, id_tok);

    // If the next token is a :, then this is a declaration.
    TRY_PEEK(lexer_, lookahead);
    if (lookahead.kind == TOK_COL) {
      return ParseNamedDeclOrDefAfterID(id_tok, result);
    }

    // Parse an expression.
    std::unique_ptr<Expr> expr;
    if (!ParseExprAfterID(id_tok, expr)) return false;

    // If the next token is a =, then this is an assignment to an expression.
    TRY_PEEK(lexer_, lookahead);
    if (lookahead.kind == TOK_ASSIGN) {
      TRY_LEX(lexer_, tok);
      assert(tok.kind == TOK_ASSIGN &&
             "Expected the next token in the assignment to be =.");

      std::unique_ptr<Expr> init;
      if (!ParseExpr(init)) return false;

      auto assign = std::make_unique<Assign>(expr, init);
      result = std::move(assign);
    } else {
      // Otherwise it has to be a call stmt.
      if (expr->getKind() != NODE_CALL) {
        diag_.Err(expr->getLoc())
            << "Expressions that form statements can only be call expressions. "
            << "Found " << NodeKindToString(expr->getKind()) << ".";
        return false;
      }
      std::unique_ptr<Call> call(static_cast<Call *>(expr.release()));
      result = std::make_unique<CallStmt>(call);
    }

    // ;
    TRY_LEX(lexer_, tok);
    if (tok.kind != TOK_SEMICOL) {
      diag_.Err(tok.loc)
          << "Expected a ';' to denote the end of a return statement.";
      return false;
    }

    return true;
  }

  diag_.Err(lookahead.loc)
      << "Was not able to parse the start of this statement.";
  return false;
}

bool Parser::ParseReturn(std::unique_ptr<Return> &result) {
  Token tok;
  assert(lexer_.Lex(tok) && tok.kind == TOK_RET &&
         "Only call ParseReturn if the previous lookahead was a return token.");
  SourceLocation retloc = tok.loc;

  std::unique_ptr<Expr> expr;
  if (!ParseExpr(expr)) return false;
  result = std::make_unique<Return>(retloc, expr);

  // ;
  TRY_LEX(lexer_, tok);
  if (tok.kind != TOK_SEMICOL) {
    diag_.Err(tok.loc)
        << "Expected a ';' to denote the end of a return statement.";
    return false;
  }

  return true;
}

bool Parser::ParseWhile(std::unique_ptr<While> &result) {
  Token tok;
  TRY_LEX(lexer_, tok);
  assert(tok.kind == TOK_WHILE && "Expected a while.");
  SourceLocation whileloc = tok.loc;

  std::unique_ptr<Expr> cond;
  if (!ParseExpr(cond)) return false;

  // { ... }
  std::vector<std::unique_ptr<Stmt>> stmts;
  if (!EnterScopeAndParseBracedStmts(stmts)) return false;

  result = std::make_unique<While>(whileloc, cond, stmts);
  return true;
}

bool Parser::ParseIf(std::unique_ptr<If> &result) {
  Token tok;
  TRY_LEX(lexer_, tok);
  assert(tok.kind == TOK_IF && "Expected an if.");
  SourceLocation ifloc = tok.loc;

  std::unique_ptr<Expr> cond;
  if (!ParseExpr(cond)) return false;

  // { ... }
  std::vector<std::unique_ptr<Stmt>> stmts;
  if (!EnterScopeAndParseBracedStmts(stmts)) return false;

  TRY_PEEK(lexer_, tok);
  if (tok.kind != TOK_ELSE) {
    // Exit early.
    result = std::make_unique<If>(ifloc, cond, stmts);
    return true;
  }

  // Handle the else case.
  TRY_LEX(lexer_, tok);  // Consume 'else'.

  // { ... }
  std::vector<std::unique_ptr<Stmt>> else_stmts;
  if (!EnterScopeAndParseBracedStmts(stmts)) return false;

  result = std::make_unique<If>(ifloc, cond, stmts, else_stmts);
  return true;
}

bool Parser::ParseParam(std::unique_ptr<Param> &result) {
  Token tok;
  TRY_LEX(lexer_, tok);
  assert(tok.kind == TOK_ID && "Expected an ID");
  std::string name = tok.chars;
  SourceLocation loc = tok.loc;

  TRY_LEX(lexer_, tok);
  if (tok.kind != TOK_COL) {
    diag_.Err(loc) << "Expected a : after the ID in a parameter.";
    return false;
  }

  std::unique_ptr<TypeNode> type;
  if (!ParseTypeNode(type)) return false;

  result = std::make_unique<Param>(loc, name, type);
  return true;
}

bool Parser::ParseNamedDeclOrDef(std::unique_ptr<Stmt> &result) {
  Token id_tok;
  TRY_LEX(lexer_, id_tok);
  assert(id_tok.kind == TOK_ID && "Expected an ID");
  return ParseNamedDeclOrDefAfterID(id_tok, result);
}

bool Parser::ParseNamedDeclOrDefAfterID(const Token &id_tok,
                                        std::unique_ptr<Stmt> &result) {
  std::string name = id_tok.chars;
  SourceLocation loc = id_tok.loc;

  if (getContext().ImmediateVarExists(name)) {
    diag_.Err(loc) << "Variable '" << name
                   << "' aleady exists in the current scope.";
    return false;
  }

  Token tok;
  TRY_LEX(lexer_, tok);
  if (tok.kind != TOK_COL) {
    diag_.Err(loc) << "Expected a : after the ID in a variable declaration.";
    return false;
  }

  std::unique_ptr<TypeNode> type_node;
  if (!ParseTypeNode(type_node)) return false;

  // Even if we fail here, it is fine since we don't reuse the context.
  std::unique_ptr<Type> type = type_node->toType();
  getContext().addVar(name, *type);

  auto decl = std::make_unique<VarDecl>(loc, name, type_node);

  // ;
  TRY_PEEK(lexer_, tok);
  if (tok.kind == TOK_SEMICOL) {
    TRY_LEX(lexer_, tok);
    result = std::move(decl);
    return true;
  }

  // =
  if (tok.kind == TOK_ASSIGN) {
    TRY_LEX(lexer_, tok);
    std::unique_ptr<Expr> init;
    if (!ParseExpr(init)) return false;

    std::unique_ptr<Type> decl_type = decl->getType();
    std::unique_ptr<Type> init_type = init->getType();
    if (*decl_type != *init_type) {
      diag_.Err(decl->getLoc())
          << "The initializer type (" << init_type->toString()
          << ") does not match the declaration type (" << decl_type->toString()
          << ").";
      return false;
    }

    result = std::make_unique<VarDef>(decl, init);

    // ;
    TRY_LEX(lexer_, tok);
    if (tok.kind != TOK_SEMICOL) {
      diag_.Err(tok.loc)
          << "Expected a ';' to denote the end of a variable definition.";
      return false;
    }

    return true;
  }

  // {
  // This must be a function defitnition.
  if (tok.kind == TOK_LBRACE) {
    if (decl->getTypeNode().getKind() != NODE_FUNC_TYPE) {
      diag_.Err(decl->getLoc())
          << "Expected the definition to be attached to a function type.";
      return false;
    }
    const FuncTypeNode &functype = decl->getTypeNode().getAs<FuncTypeNode>();

    // { ... }
    std::vector<std::unique_ptr<Stmt>> stmts;
    if (!EnterScopeAndParseFuncBracedStmts(functype, stmts)) return false;

    result = std::make_unique<FuncDef>(decl, stmts);
    return true;
  }

  diag_.Err(tok.loc)
      << "Expected a ';' to indicate a variable declaration, '=' to indicate a "
         "variable definition, or start of a function definition.";
  return false;
}

bool Parser::ParsePtrTypeNode(std::unique_ptr<TypeNode> &type,
                              std::unique_ptr<TypeNode> &result) {
  Token tok;
  TRY_LEX(lexer_, tok);
  assert(tok.kind == TOK_PTR && "Expected the next token to be a '*'");
  result = std::make_unique<PtrTypeNode>(type->getLoc(), type);
  return true;
}

bool Parser::ParseArrayTypeNode(std::unique_ptr<TypeNode> &type,
                                std::unique_ptr<TypeNode> &result) {
  Token tok;
  TRY_LEX(lexer_, tok);
  assert(tok.kind == TOK_LBRACK && "Expected the next token to be a '['");

  TRY_LEX(lexer_, tok);
  if (tok.kind != TOK_INT) {
    diag_.Err(tok.loc) << "Expected the array size to be an integer literal.";
    return false;
  }

  std::unique_ptr<Int> int_val = Int::fromToken(tok);
  if (!int_val->getVal()) {
    diag_.Err(tok.loc) << "Cannot declare an empty array type.";
    return false;
  }
  result =
      std::make_unique<ArrayTypeNode>(type->getLoc(), type, int_val->getVal());

  TRY_LEX(lexer_, tok);
  if (tok.kind != TOK_RBRACK) {
    diag_.Err(tok.loc) << "Expected the array type to end with a ']'.";
    return false;
  }
  return true;
}

bool Parser::ParseTypeNode(std::unique_ptr<TypeNode> &result) {
  Token lookahead;
  TRY_PEEK(lexer_, lookahead);
  if (lookahead.kind == TOK_LPAR) {
    std::unique_ptr<FuncTypeNode> func_type_node;
    if (!ParseFuncTypeNode(func_type_node)) return false;
    result = std::move(func_type_node);
    return true;
  }

  // Parse an ID type node, which is just a simple ID.
  Token tok;
  TRY_LEX(lexer_, tok);
  if (tok.kind != TOK_ID) {
    diag_.Err(tok.loc) << "Expected an ID for the type of this node.";
    return false;
  }

  const Type *canonical_type = getContext().getType(tok.chars);
  if (!canonical_type) {
    diag_.Err(tok.loc) << "Unknown type '" << tok.chars << "'.";
    return false;
  }
  std::unique_ptr<Type> type_clone = canonical_type->Clone();
  result = std::make_unique<IDTypeNode>(tok.loc, tok.chars, type_clone);

  bool parse_type_modifiers = true;
  while (parse_type_modifiers) {
    TRY_PEEK(lexer_, lookahead);

    switch (lookahead.kind) {
      case TOK_PTR:
        // Check for pointers
        if (!ParsePtrTypeNode(result, result)) return false;
        break;
      case TOK_LBRACK:
        // Check for arrays
        if (!ParseArrayTypeNode(result, result)) return false;
        break;
      default:
        parse_type_modifiers = false;
        break;
    }
  }

  return true;
}

bool Parser::ParseFuncTypeNode(std::unique_ptr<FuncTypeNode> &result) {
  Token tok;
  TRY_LEX(lexer_, tok);
  assert(tok.kind == TOK_LPAR && "Expected a (");
  SourceLocation loc = tok.loc;

  // Parse arguments.
  std::vector<std::unique_ptr<Param>> params;
  bool isvararg = false;

  Token lookahead;
  TRY_PEEK(lexer_, lookahead);
  if (lookahead.kind != TOK_RPAR) {
    // Arguments.
    while (1) {
      // Consume and read statements.
      TRY_PEEK(lexer_, tok);
      if (tok.kind == TOK_VARARG) {
        TRY_LEX(lexer_, tok);
        if (isvararg) {
          // Was declared twice.
          diag_.Err(tok.loc) << "Can only declare variadic arguments once.";
          return false;
        }
        isvararg = true;

        TRY_PEEK(lexer_, tok);
        if (tok.kind != TOK_RPAR) {
          diag_.Err(tok.loc) << "If there is a variadic argument, it can only "
                                "be the last argument in the function.";
          return false;
        }
        break;
      } else if (tok.kind != TOK_ID) {
        diag_.Err(tok.loc)
            << "Expected either a function argument (as an ID) or a closing "
               "parethesis when parsing a function definition.";
        return false;
      }

      std::unique_ptr<Param> param;
      if (!ParseParam(param)) return false;
      params.push_back(std::move(param));

      TRY_PEEK(lexer_, lookahead);
      if (lookahead.kind != TOK_RPAR) {
        TRY_LEX(lexer_, tok);
        if (tok.kind != TOK_COMMA) {
          diag_.Err(tok.loc) << "Expected a ',' to separate arguments "
                                "or a closing parenthesis when parsing the "
                                "arguments of a function type.";
          return false;
        }
      } else {
        break;
      }
    }
  }

  // )
  TRY_LEX(lexer_, tok);
  assert(tok.kind == TOK_RPAR &&
         "We should have only broken out of the previous loop if we ran into a "
         "closing parenthesis.");

  // ->
  TRY_LEX(lexer_, tok);
  if (tok.kind != TOK_RARROW) {
    diag_.Err(tok.loc) << "Expected -> to indicate the return type.";
    return false;
  }

  std::unique_ptr<TypeNode> ret_type;
  if (!ParseTypeNode(ret_type)) return false;

  result = std::make_unique<FuncTypeNode>(loc, ret_type, params, isvararg);
  return true;
}

bool Parser::ParseExprAfterID(const Token &id_tok,
                              std::unique_ptr<Expr> &result) {
  assert(id_tok.kind == TOK_ID &&
         "Expected the already lexed token to be an ID.");
  if (!ParseSingleExprAfterID(id_tok, result)) return false;
  return TryToParseCompoundExpr(result);
}

static bool BinOpCodeFromTokenKind(TokenKind kind, BinOpCode &op) {
  switch (kind) {
    case TOK_LT:
      op = BINOP_LT;
      return true;
    case TOK_LE:
      op = BINOP_LE;
      return true;
    case TOK_ADD:
      op = BINOP_ADD;
      return true;
    case TOK_SUB:
      op = BINOP_SUB;
      return true;
    case TOK_EQ:
      op = BINOP_EQ;
      return true;
    default:
      return false;
  }
}

bool Parser::TryToParseCompoundExpr(std::unique_ptr<Expr> &expr) {
  while (1) {
    Token tok;
    TRY_PEEK(lexer_, tok);
    BinOpCode op;
    if (BinOpCodeFromTokenKind(tok.kind, op)) {
      TRY_LEX(lexer_, tok);
      std::unique_ptr<Expr> rhs;
      if (!ParseExpr(rhs)) return false;
      expr = std::make_unique<BinOp>(expr, rhs, op);
      continue;
    } else if (tok.kind == TOK_MEMBER_ACCESS) {
      TRY_LEX(lexer_, tok);  // .
      TRY_LEX(lexer_, tok);
      if (tok.kind != TOK_ID) {
        diag_.Err(tok.loc) << "Expected the name of the member to access.";
        return false;
      }

      std::unique_ptr<Type> exprtype = expr->getType();

      if (exprtype->getKind() != TYPE_STRUCT) {
        diag_.Err(expr->getLoc())
            << "Expected the expression to be a struct type. Found '"
            << TypeKindToString(exprtype->getKind()) << "'";
        return false;
      }

      expr = std::make_unique<MemberAccess>(expr, tok.chars);
      continue;
    } else if (tok.kind == TOK_LPAR) {
      if (!TryToMakeCallAfterExpr(expr)) return false;
      continue;
    } else if (tok.kind == TOK_LBRACK) {
      TRY_LEX(lexer_, tok);  // [
      SourceLocation arrayloc = tok.loc;

      std::unique_ptr<Expr> idx;
      if (!ParseExpr(idx)) return false;

      TRY_LEX(lexer_, tok);
      if (tok.kind != TOK_RBRACK) {
        diag_.Err(tok.loc)
            << "Expected the closing ']' for an array subscript.";
        return false;
      }

      expr = std::make_unique<Subscript>(expr, idx);
      continue;
    }
    break;
  }

  return true;
}

EnumType EnumDef::getEnumType() const { return EnumType(values_, num_bits_); }

StructType TypeDef::getStructType() const {
  StructType structtype;
  for (const auto &member_ptr : members_) {
    if (member_ptr->getKind() == NODE_MEMBER_VARDECL) {
      const VarDecl &vardecl = member_ptr->getAs<MemberVarDecl>().getVarDecl();
      auto type = vardecl.getType();
      structtype.addMember(vardecl.getName(), type);
    } else {
      UNREACHABLE("Unexpected member decl.");
    }
  }
  return structtype;
}

bool Parser::TryToMakeCallAfterExpr(std::unique_ptr<Expr> &expr) {
  Token tok;
  TRY_PEEK(lexer_, tok);
  if (tok.kind == TOK_LPAR) {
    if (expr->getType()->getKind() != TYPE_FUNC) {
      diag_.Err(tok.loc)
          << "The expression we are calling must be a function type.";
      return false;
    }

    // Parse the start of a call expression.
    TRY_LEX(lexer_, tok);

    // Parse arguments.
    std::vector<std::unique_ptr<Expr>> args;

    Token lookahead;
    TRY_PEEK(lexer_, lookahead);
    if (lookahead.kind != TOK_RPAR) {
      // Arguments.
      while (1) {
        // Consume and read expressions.
        std::unique_ptr<Expr> arg;
        if (!ParseExpr(arg)) return false;
        args.push_back(std::move(arg));

        TRY_PEEK(lexer_, lookahead);
        if (lookahead.kind != TOK_RPAR) {
          TRY_LEX(lexer_, tok);
          if (tok.kind != TOK_COMMA) {
            diag_.Err(tok.loc) << "Expected a ',' to separate arguments "
                                  "or a closing parenthesis when parsing the "
                                  "arguments of a function call.";
            return false;
          }
        } else {
          break;
        }
      }
    }

    // )
    TRY_LEX(lexer_, tok);
    assert(
        tok.kind == TOK_RPAR &&
        "We should have only broken out of the previous loop if we ran into a "
        "closing parenthesis.");

    expr = std::make_unique<Call>(expr, args);
  }
  return true;
}

bool Parser::ParseExpr(std::unique_ptr<Expr> &result) {
  if (!ParseSingleExpr(result)) return false;
  return TryToParseCompoundExpr(result);
}

bool Parser::ParseSingleExprAfterID(const Token &id_tok,
                                    std::unique_ptr<Expr> &result) {
  assert(id_tok.kind == TOK_ID &&
         "Expected the already lexed token to be an ID.");

  // Parse a caller for now.
  SourceLocation exprloc = id_tok.loc;
  std::string name = id_tok.chars;

  auto *type = getTypeForVar(name);
  if (!type) {
    diag_.Err(exprloc) << "Unknown variable '" << name << "'.";
    return false;
  }
  if (getContext().isEnumLiteral(name)) {
    const EnumType &enum_type = type->getAs<EnumType>();
    size_t value;
    assert(enum_type.getValue(name, value) &&
           "Expected this variable to be part of the enum type.");
    std::unique_ptr<EnumLiteral> literal(
        new EnumLiteral(exprloc, name, value, *type));
    result = std::move(literal);
  } else {
    std::unique_ptr<ID> caller(new ID(exprloc, name, *type));
    result = std::move(caller);
  }
  return true;
}

bool Parser::ParseStr(std::unique_ptr<Str> &result) {
  Token tok;
  TRY_LEX(lexer_, tok);
  assert(tok.kind == TOK_STR && "Expected a string token to parse.");
  result = std::make_unique<Str>(tok.loc, tok.chars);
  return true;
}

template <typename T>
static T ParseInt(const std::string_view &str,
                  unsigned *num_chars_parsed = nullptr) {
  T result = 0;
  unsigned i = 0;
  assert(isdigit(str.front()));
  do {
    result *= 10;
    result += str[i] - '0';
    ++i;
  } while (i < str.size() && isdigit(str[i]));
  if (num_chars_parsed) *num_chars_parsed = i;
  return result;
}

// TODO: Make a new function that gets the value and number of bits instead of
// always having to create a new Int.
std::unique_ptr<Int> Int::fromToken(const Token &tok) {
  const std::string &str = tok.chars;
  assert(!str.empty());

  unsigned i;
  int64_t result = ParseInt<int64_t>(str, &i);
  unsigned num_bits;
  if (str[i] == 'i') {
    ++i;
    num_bits = ParseInt<unsigned>(std::string_view(str.c_str() + i));
  } else if (i == str.size()) {
    num_bits = kDefaultIntNumBits;
  } else {
    UNREACHABLE("Did not finish parsing the whole string.");
  }

  return std::make_unique<Int>(tok.loc, result, num_bits);
}

bool Parser::ParseArray(std::unique_ptr<Expr> &result) {
  Token tok;
  TRY_LEX(lexer_, tok);
  assert(tok.kind == TOK_LBRACK &&
         "Do not call ParseArray unless the next token is a left bracket.");
  SourceLocation arrayloc = tok.loc;

  // Parse arguments.
  std::vector<std::unique_ptr<Expr>> elems;

  Token lookahead;
  TRY_PEEK(lexer_, lookahead);
  if (lookahead.kind != TOK_RBRACK) {
    // Arguments.
    while (1) {
      // Consume and read expressions.
      std::unique_ptr<Expr> elem;
      if (!ParseExpr(elem)) return false;
      elems.push_back(std::move(elem));

      TRY_PEEK(lexer_, lookahead);
      if (lookahead.kind != TOK_RBRACK) {
        TRY_LEX(lexer_, tok);
        if (tok.kind != TOK_COMMA) {
          diag_.Err(tok.loc) << "Expected a ',' to separate array elements "
                                "or a closing ']' when parsing the "
                                "elements of an array.";
          return false;
        }
      } else {
        break;
      }
    }
  }

  // ]
  TRY_LEX(lexer_, tok);
  assert(tok.kind == TOK_RBRACK &&
         "We should have only broken out of the previous loop if we ran into a "
         "closing bracket.");

  result = std::make_unique<Array>(arrayloc, elems);
  return true;
}

bool Parser::ParseSingleExpr(std::unique_ptr<Expr> &result) {
  Token lookahead, tok;
  TRY_PEEK(lexer_, lookahead);

  switch (lookahead.kind) {
    case TOK_INT: {
      TRY_LEX(lexer_, tok);
      result = Int::fromToken(tok);
      return true;
    }
    case TOK_STR: {
      std::unique_ptr<Str> str;
      if (!ParseStr(str)) return false;
      result = std::move(str);
      return true;
    }
    case TOK_ID: {
      TRY_LEX(lexer_, tok);
      return ParseSingleExprAfterID(tok, result);
    }
    case TOK_TRUE: {
      TRY_LEX(lexer_, tok);
      result = std::make_unique<Bool>(tok.loc, true);
      return true;
    }
    case TOK_FALSE: {
      TRY_LEX(lexer_, tok);
      result = std::make_unique<Bool>(tok.loc, false);
      return true;
    }
    case TOK_LBRACK: {
      return ParseArray(result);
    }
    default:
      diag_.Err(lookahead.loc) << "Expected an expression. Found "
                               << TokenKindAsString(lookahead.kind) << ".";
      return false;
  }
}

// We check for is_str, since we won't be returning an underlying type in the
// event it is a string type.
static void GetPointerLikeAttributes(const Type &type, bool &has_size,
                                     size_t &size, const Type *&underlying_type,
                                     bool &is_str) {
  if (const auto *ptr = type.maybeAs<PtrType>()) {
    has_size = false;
    is_str = false;
    underlying_type = &ptr->getPointeeType();
  } else if (const auto *str = type.maybeAs<StrType>()) {
    has_size = true;
    size = str->getSize();
    is_str = true;
  } else if (const auto *arr = type.maybeAs<ArrayType>()) {
    has_size = true;
    size = arr->getNumElems();
    is_str = false;
    underlying_type = &arr->getElementType();
  } else {
    UNREACHABLE("Unexpected pointer-like type");
  }
}

static bool PointerLikeTypesAreEqual(const Type &type1, const Type &type2) {
  bool type1_has_size, type2_has_size, is_str1, is_str2;
  size_t size1, size2;
  const Type *underlying_type1, *underlying_type2;
  GetPointerLikeAttributes(type1, type1_has_size, size1, underlying_type1,
                           is_str1);
  GetPointerLikeAttributes(type2, type2_has_size, size2, underlying_type2,
                           is_str2);

  if (is_str1) {
    // 1 is a string
    if (is_str2) {
      // 2 is a string
      return size1 == size2;
    } else if (type1_has_size) {
      // 2 is an array
      if (const auto *int_type = underlying_type2->maybeAs<IntType>()) {
        return int_type->getNumBits() == kNumCharBits && size1 == size2;
      }
      return false;
    } else {
      // 2 is a ptr
      if (const auto *pointee = underlying_type2->maybeAs<IntType>()) {
        return pointee->getNumBits() == kNumCharBits;
      }
      return false;
    }
  } else if (type1_has_size) {
    // 1 is an array
    if (is_str2) {
      // 2 is a string
      if (const auto *int_type = underlying_type1->maybeAs<IntType>()) {
        return int_type->getNumBits() == kNumCharBits && size1 == size2;
      }
      return false;
    } else if (type1_has_size) {
      // 2 is an array
      return size1 == size2 && (*underlying_type1 == *underlying_type2);
    } else {
      // 2 is a ptr
      return *underlying_type1 == *underlying_type2;
    }
  } else {
    // 1 is a pointer
    if (is_str2) {
      // 2 is a string
      if (const auto *int_type = underlying_type1->maybeAs<IntType>()) {
        return int_type->getNumBits() == kNumCharBits;
      }
      return false;
    } else {
      // 2 is an array or pointer
      return *underlying_type1 == *underlying_type2;
    }
  }
}

bool PtrType::isEqual(const Type &other) const {
  return PointerLikeTypesAreEqual(*this, other);
}

bool StrType::isEqual(const Type &other) const {
  return PointerLikeTypesAreEqual(*this, other);
}

bool ArrayType::isEqual(const Type &other) const {
  return PointerLikeTypesAreEqual(*this, other);
}

std::unordered_map<std::string, std::unique_ptr<Type>> Context::cached_types_;

#define TYPE(EnumKind, Class) TypeKind Class::Kind = EnumKind;
#include "Types.def"

}  // namespace qwip
