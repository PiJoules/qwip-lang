#include "Parser.h"

// Convenience macros for lexing and exiting a method on a lex error.
#define TRY_LEX(Lexer, Tok) \
  if (!Lexer.Lex(Tok)) return false;

#define TRY_PEEK(Lexer, Tok) \
  if (!Lexer.Peek(Tok)) return false;

namespace qwip {

bool Parser::Parse(std::unique_ptr<Module> &result) {
  return ParseModule(result);
}

bool Parser::ParseModule(std::unique_ptr<Module> &result) {
  Token lookahead;
  TRY_PEEK(lexer_, lookahead);
  EnterScope();

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
    std::unique_ptr<VarDef> vardecl(static_cast<VarDef *>(func.release()));
    result = std::make_unique<ExternVarDef>(vardecl);
  } else {
    std::unique_ptr<VarDecl> vardecl(static_cast<VarDecl *>(func.release()));
    result = std::make_unique<ExternVarDecl>(vardecl);
  }

  return true;
}

bool Parser::ParseExternTypeDef(std::unique_ptr<ExternTypeDef> &result) {
  Token tok;
  TRY_LEX(lexer_, tok);  // type
  CHECK(tok.kind == TOK_TYPE,
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
    if (nameddecl->getKind() == NODE_VARDEF) {
      std::unique_ptr<VarDef> vardef(
          static_cast<VarDef *>(nameddecl.release()));
      members.push_back(std::make_unique<MemberVarDef>(vardef));
    } else if (nameddecl->getKind() == NODE_FUNCDEF) {
      std::unique_ptr<FuncDef> funcdef(
          static_cast<FuncDef *>(nameddecl.release()));
      members.push_back(std::make_unique<MemberFuncDef>(funcdef));
    } else {
      diag_.Err(nameddecl->getLoc())
          << "A new struct can only contain definitions.";
      return false;
    }
  }

  // }
  TRY_LEX(lexer_, tok);
  CHECK(tok.kind == TOK_RBRACE,
        "Should only break out of the previous loop if we ran into a closing "
        "brace.");

  auto type_def = std::make_unique<TypeDef>(typeloc, name, members);
  result = std::make_unique<ExternTypeDef>(type_def);
  return true;
}

bool Parser::ParseBracedStmts(std::vector<std::unique_ptr<Stmt>> &stmts) {
  // {
  Token tok;
  TRY_LEX(lexer_, tok);
  CHECK(tok.kind == TOK_LBRACE,
        "Do not call ParseBracedStmts unless the previous lookahead was an "
        "opening brace.");

  EnterScope();

  // Statements
  while (1) {
    Token lookahead;
    TRY_PEEK(lexer_, lookahead);

    if (lookahead.kind == TOK_RBRACE) break;

    // Consume and read statements.
    std::unique_ptr<Stmt> stmt;
    if (!ParseStmt(stmt)) {
      ExitScope();
      return false;
    }
    stmts.push_back(std::move(stmt));
  }

  // }
  TRY_LEX(lexer_, tok);
  CHECK(tok.kind == TOK_RBRACE,
        "Should only break out of the previous loop if we ran into a closing "
        "brace.");

  ExitScope();
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
    if (lookahead.kind == TOK_ASSIGN) {
      TRY_LEX(lexer_, tok);
      CHECK(tok.kind == TOK_ASSIGN,
            "Expected the next token in the assignment to be =.");

      std::unique_ptr<Expr> init;
      if (!ParseExpr(init)) return false;

      auto assign = std::make_unique<Assign>(expr, init);
      result = std::move(assign);
    } else {
      // Otherwise it has to be a call stmt.
      if (expr->getKind() != NODE_CALL) {
        diag_.Err(expr->getLoc())
            << "Expressions that form statements can only be call expressions.";
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
  CHECK(lexer_.Lex(tok) && tok.kind == TOK_RET,
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
  CHECK(tok.kind == TOK_WHILE, "Expected a while.");
  SourceLocation whileloc = tok.loc;

  std::unique_ptr<Expr> cond;
  if (!ParseExpr(cond)) return false;

  // { ... }
  std::vector<std::unique_ptr<Stmt>> stmts;
  if (!ParseBracedStmts(stmts)) return false;

  result = std::make_unique<While>(whileloc, cond, stmts);
  return true;
}

bool Parser::ParseIf(std::unique_ptr<If> &result) {
  Token tok;
  TRY_LEX(lexer_, tok);
  CHECK(tok.kind == TOK_IF, "Expected an if.");
  SourceLocation ifloc = tok.loc;

  std::unique_ptr<Expr> cond;
  if (!ParseExpr(cond)) return false;

  // { ... }
  std::vector<std::unique_ptr<Stmt>> stmts;
  if (!ParseBracedStmts(stmts)) return false;

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
  if (!ParseBracedStmts(stmts)) return false;

  result = std::make_unique<If>(ifloc, cond, stmts, else_stmts);
  return true;
}

bool Parser::ParseParam(std::unique_ptr<Param> &result) {
  Token tok;
  TRY_LEX(lexer_, tok);
  CHECK(tok.kind == TOK_ID, "Expected an ID");
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
  CHECK(id_tok.kind == TOK_ID, "Expected an ID");
  return ParseNamedDeclOrDefAfterID(id_tok, result);
}

bool Parser::ParseNamedDeclOrDefAfterID(const Token &id_tok,
                                        std::unique_ptr<Stmt> &result) {
  std::string name = id_tok.chars;
  SourceLocation loc = id_tok.loc;

  Token tok;
  TRY_LEX(lexer_, tok);
  if (tok.kind != TOK_COL) {
    diag_.Err(loc) << "Expected a : after the ID in a variable declaration.";
    return false;
  }

  std::unique_ptr<TypeNode> type;
  if (!ParseTypeNode(type)) return false;
  auto decl = std::make_unique<VarDecl>(loc, name, type);

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
    // { ... }
    std::vector<std::unique_ptr<Stmt>> stmts;
    if (!ParseBracedStmts(stmts)) return false;

    result = std::make_unique<FuncDef>(decl, stmts);
    return true;
  }

  diag_.Err(tok.loc)
      << "Expected a ';' to indicate a variable declaration, '=' to indicate a "
         "variable definition, or start of a function definition.";
  return false;
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

  result = std::make_unique<IDTypeNode>(tok.loc, tok.chars);

  // Check for pointers
  TRY_PEEK(lexer_, lookahead);
  while (lookahead.kind == TOK_PTR) {
    result = std::make_unique<PtrTypeNode>(tok.loc, result);
    TRY_LEX(lexer_, tok);
    TRY_PEEK(lexer_, lookahead);
  }

  return true;
}

bool Parser::ParseFuncTypeNode(std::unique_ptr<FuncTypeNode> &result) {
  Token tok;
  TRY_LEX(lexer_, tok);
  CHECK(tok.kind == TOK_LPAR, "Expected a (");
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
  CHECK(tok.kind == TOK_RPAR,
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
  CHECK(id_tok.kind == TOK_ID, "Expected the already lexed token to be an ID.");
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
            << "Expected the expression to be a struct type.";
        return false;
      }

      expr = std::make_unique<MemberAccess>(expr, tok.chars);
      continue;
    } else if (tok.kind == TOK_LPAR) {
      if (!TryToMakeCallAfterExpr(expr)) return false;
      continue;
    }
    break;
  }

  return true;
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
    CHECK(
        tok.kind == TOK_RPAR,
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
  CHECK(id_tok.kind == TOK_ID, "Expected the already lexed token to be an ID.");

  // Parse a caller for now.
  SourceLocation exprloc = id_tok.loc;
  std::string name = id_tok.chars;

  auto *type = getContext().getType(name);
  if (!type) {
    diag_.Err(exprloc) << "Unknown variable '" << name << "'.";
    return false;
  }

  std::unique_ptr<ID> caller(new ID(exprloc, name, *type));
  result = std::move(caller);
  return true;
}

bool Parser::ParseStr(std::unique_ptr<Str> &result) {
  Token tok;
  TRY_LEX(lexer_, tok);
  CHECK(tok.kind == TOK_STR, "Expected a string token to parse.");
  result = std::make_unique<Str>(tok.loc, tok.chars);
  return true;
}

bool Parser::ParseSingleExpr(std::unique_ptr<Expr> &result) {
  Token lookahead, tok;
  TRY_PEEK(lexer_, lookahead);

  switch (lookahead.kind) {
    case TOK_INT: {
      TRY_LEX(lexer_, tok);
      int val = std::stoi(tok.chars);
      result = std::make_unique<Int>(tok.loc, val);
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
    default:
      diag_.Err(lookahead.loc) << "Expected an expression. Found "
                               << TokenKindAsString(lookahead.kind) << ".";
      return false;
  }
}

}  // namespace qwip
