#include "Parser.h"

// Convenience macros for lexing and exiting a method on a lex error.
#define TRY_LEX(Lexer, Tok) \
  if (!Lexer.Lex(Tok)) return false;

#define TRY_PEEK(Lexer, Tok) \
  if (!Lexer.Peek(Tok)) return false;

namespace qwip {

bool Parser::ParseModule(std::unique_ptr<Module> &result) {
  Token lookahead;
  TRY_PEEK(lexer_, lookahead);

  std::vector<std::unique_ptr<ExternDecl>> decls;
  while (lookahead.kind != TOK_EOF) {
    if (lookahead.kind == TOK_ID) {
      std::unique_ptr<ExternDecl> externdecl;
      if (!ParseExternDecl(externdecl)) return false;
      decls.push_back(std::move(externdecl));
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

bool Parser::ParseExternDecl(std::unique_ptr<ExternDecl> &result) {
  Token tok;
  TRY_LEX(lexer_, tok);
  CHECK(tok.kind == TOK_ID,
        "Expected the first token when parsing an external declaration to be "
        "an ID.");
  SourceLocation declloc = tok.loc;
  std::string declname = tok.chars;

  // :
  TRY_LEX(lexer_, tok);
  if (tok.kind != TOK_COL) {
    diag_.Err(tok.loc) << "Expected a : to indicate the start of the type";
    return false;
  }

  // Parse the type of the declaration. If it is a function, it will start with
  // a (. Otherwise it is a normal variable declaration.

  // (
  TRY_PEEK(lexer_, tok);
  if (tok.kind != TOK_LPAR) {
    diag_.Err(tok.loc) << "Can only declare functions for now.";
    return false;
  }

  std::unique_ptr<FuncTypeNode> func_type;
  if (!ParseFuncTypeNode(func_type)) return false;

  // { ... }
  std::vector<std::unique_ptr<Stmt>> stmts;
  if (!ParseBracedStmts(stmts)) return false;

  auto funcdecl = std::make_unique<FuncDecl>(declloc, declname, func_type);
  result = std::make_unique<FuncDef>(funcdecl, stmts);
  return true;
}

bool Parser::ParseBracedStmts(std::vector<std::unique_ptr<Stmt>> &stmts) {
  // {
  Token tok;
  TRY_LEX(lexer_, tok);
  CHECK(tok.kind == TOK_LBRACE,
        "Do not call ParseBracedStmts unless the previous lookahead was an "
        "opening brace.");

  // Statements
  while (1) {
    Token lookahead;
    TRY_PEEK(lexer_, lookahead);

    if (lookahead.kind == TOK_RBRACE) break;

    // Consume and read statements.
    std::unique_ptr<Stmt> stmt;
    if (!ParseStmt(stmt)) return false;
    stmts.push_back(std::move(stmt));
  }

  // }
  TRY_LEX(lexer_, tok);
  CHECK(tok.kind == TOK_RBRACE,
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
  } else if (lookahead.kind == TOK_ID) {
    // Parse an expression statement or a variable declaration.
    Token id_tok;
    TRY_LEX(lexer_, id_tok);

    // If the next token is a :, then this is a variable declaration.
    TRY_PEEK(lexer_, lookahead);
    if (lookahead.kind == TOK_COL) {
      std::unique_ptr<VarDecl> decl;
      if (!ParseVarDeclAfterID(id_tok, decl)) return false;
      result = std::move(decl);
      return true;
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

bool Parser::ParseVarDeclAfterID(const Token &id_tok,
                                 std::unique_ptr<VarDecl> &result) {
  CHECK(id_tok.kind == TOK_ID, "Expected an ID");
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

  // Check for an initial value.
  std::unique_ptr<Expr> init;
  TRY_PEEK(lexer_, tok);
  if (tok.kind == TOK_ASSIGN) {
    TRY_LEX(lexer_, tok);
    if (!ParseExpr(init)) return false;
  }

  // ;
  TRY_LEX(lexer_, tok);
  if (tok.kind != TOK_SEMICOL) {
    diag_.Err(tok.loc)
        << "Expected a ';' to denote the end of a return statement.";
    return false;
  }

  result = std::make_unique<VarDecl>(loc, name, type, init);
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

  Token lookahead;
  TRY_PEEK(lexer_, lookahead);
  if (lookahead.kind != TOK_RPAR) {
    // Arguments.
    while (1) {
      // Consume and read statements.
      TRY_PEEK(lexer_, tok);
      if (tok.kind != TOK_ID) {
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

  result = std::make_unique<FuncTypeNode>(loc, ret_type, params);
  return true;
}

bool Parser::ParseExprAfterID(const Token &id_tok,
                              std::unique_ptr<Expr> &result) {
  CHECK(id_tok.kind == TOK_ID, "Expected the already lexed token to be an ID.");
  if (!ParseSingleExprAfterID(id_tok, result)) return false;
  return TryToMakeCallAfterExpr(result);
}

bool Parser::TryToMakeCallAfterExpr(std::unique_ptr<Expr> &expr) {
  Token tok;
  TRY_PEEK(lexer_, tok);
  if (tok.kind == TOK_LPAR) {
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
  return TryToMakeCallAfterExpr(result);
}

bool Parser::ParseSingleExprAfterID(const Token &id_tok,
                                    std::unique_ptr<Expr> &result) {
  CHECK(id_tok.kind == TOK_ID, "Expected the already lexed token to be an ID.");

  // Parse a caller for now.
  SourceLocation exprloc = id_tok.loc;
  std::string name = id_tok.chars;
  std::unique_ptr<ID> caller(new ID(exprloc, name));
  result = std::move(caller);
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
      TRY_LEX(lexer_, tok);
      result = std::make_unique<Str>(tok.loc, tok.chars);
      return true;
    }
    case TOK_ID: {
      TRY_LEX(lexer_, tok);
      return ParseSingleExprAfterID(tok, result);
    }
    default:
      diag_.Err(lookahead.loc) << "Expected an expression.";
      return false;
  }
}

}  // namespace qwip
