#include "Lexer.h"

#include <unordered_map>

namespace qwip {

std::string TokenKindAsString(TokenKind kind) {
  switch (kind) {
#define TOKEN(Kind) \
  case Kind:        \
    return STR(Kind);
#include "Tokens.def"
  }
  UNREACHABLE("Unknown token");
  return "";
}

bool Lexer::Lex(Token &result) {
  if (has_lookahead_) {
    has_lookahead_ = false;
    result = lookahead_;
    return true;
  }

  std::string buffer;
  int next_char = input_.peek();
  result.loc.filename = filename_;

  // Handle EOF, whitespace, or comments. We skip these tokens.
  while (1) {
    if (next_char == EOF) {
      result.loc.line = line_;
      result.loc.col = col_;
      result.kind = TOK_EOF;
      return true;
    } else if (isspace(next_char)) {
      while (isspace(next_char)) {
        getNextChar();
        next_char = input_.peek();
      }
    } else if (next_char == '/') {
      if (!LexComment(result)) return false;

      // The result is a Comment token at this point. Exit early if we want to
      // track comments. Otherwise just continue to check for more skippable
      // tokens.
      if (lex_comments_) return true;
    } else {
      // Nothing else to skip.
      break;
    }

    next_char = input_.peek();
  }

  result.loc.line = line_;
  result.loc.col = col_;

  // Handle RARROW early since it can conflict when lexing SUB.
  if (next_char == '-') {
    getNextChar();
    next_char = input_.peek();
    if (next_char == '>') {
      getNextChar();
      result.kind = TOK_RARROW;
      result.chars = "->";
      return true;
    }
    result.kind = TOK_SUB;
    result.chars = "-";
    return true;
  } else if (next_char == '.') {
    // ... for variable arguments.
    getNextChar();  // 1 .

    next_char = input_.peek();
    if (next_char != '.') {
      result.chars = ".";
      result.kind = TOK_MEMBER_ACCESS;
      return true;
    }
    getNextChar();  // 2 ..

    if (getNextChar() != '.') {  // 3 ...
      result.loc.line = line_;
      result.loc.col = col_;
      getDiag().Err(result.loc)
          << "Expected '...' to indicate variadic arguments.";
      return false;
    }
    result.kind = TOK_VARARG;
    result.chars = "...";
    return true;
  } else if (next_char == '<') {
    getNextChar();
    next_char = input_.peek();
    if (next_char == '=') {
      getNextChar();
      result.kind = TOK_LE;
      result.chars = "<=";
      return true;
    }
    result.kind = TOK_LT;
    result.chars = "<";
    return true;
  } else if (next_char == '=') {
    getNextChar();
    next_char = input_.peek();
    if (next_char == '=') {
      getNextChar();
      result.kind = TOK_EQ;
      result.chars = "==";
      return true;
    }
    result.kind = TOK_ASSIGN;
    result.chars = "=";
    return true;
  }

  // Handle single char tokens.
  assert(next_char != EOF && "EOFs should've been handled earlier.");
  if (LexSingleCharToken(static_cast<char>(next_char), result)) return true;

  if (next_char == '"') return LexString(result);

  if (isdigit(next_char)) return LexInt(result);

  // Proceed to handle keywords.
  if (isalpha(next_char) || next_char == '_') return LexKeywordOrID(result);

  getDiag().Err(result.loc) << "Unable to lex character '" << next_char << "'.";
  return false;
}

bool Lexer::LexSingleCharToken(char lookahead, Token &result) {
  switch (lookahead) {
#define TOKEN_1CHAR(Kind, Char) \
  case Char:                    \
    result.kind = Kind;         \
    break;
#include "Tokens.def"
    default:
      return false;
  }
  assert(lookahead == getNextChar() &&
         "The lookahead passed to this should have been the next character off "
         "the stream.");
  result.chars = lookahead;
  return true;
}

bool Lexer::LexString(Token &result) {
  int c = getNextChar();
  assert(
      c == '"' &&
      "Expected a double quote as the first character off the input_ stream.");

  int next_char = input_.peek();
  result.chars.clear();
  while (next_char != '"') {
    if (next_char == '\\') {
      getNextChar();
      next_char = getNextChar();
      switch (next_char) {
        case 'n':
          result.chars.push_back('\n');
          break;
        case 't':
          result.chars.push_back('\t');
          break;
        case EOF:
          getDiag().Err(getCurrentLoc())
              << "Reached end of file. Could not finish string.";
          return false;
        default:
          result.chars.push_back(static_cast<char>(next_char));
      }
    } else {
      result.chars.push_back(static_cast<char>(getNextChar()));
    }
    next_char = input_.peek();
  }

  // next_char is "
  assert(getNextChar() == '"' && "Expected the next char to be the ending \".");
  result.kind = TOK_STR;
  return true;
}

bool Lexer::LexInt(Token &result) {
  int c = getNextChar();
  assert(isdigit(c) &&
         "Expeted the first character off the stream tp be a digit.");

  int next_char = input_.peek();
  result.chars.clear();
  result.chars.push_back(static_cast<char>(c));

  while (isdigit(next_char)) {
    result.chars.push_back(static_cast<char>(getNextChar()));
    next_char = input_.peek();
  }

  // Next char is not a digit.
  result.kind = TOK_INT;
  return true;
}

bool Lexer::LexKeywordOrID(Token &result) {
  int c = getNextChar();
  assert((isalpha(c) || c == '_') &&
         "Expected the first character in the input_ to be an underscore or "
         "letter.");

  int next_char = input_.peek();
  std::string buffer;
  buffer.push_back(static_cast<char>(c));

  while (isalpha(next_char) || isdigit(next_char) || next_char == '_') {
    buffer.push_back(static_cast<char>(getNextChar()));
    next_char = input_.peek();
  }

  if (LexKeyword(buffer, result)) return true;

  result.kind = TOK_ID;
  result.chars = buffer;
  return true;
}

bool Lexer::LexComment(Token &result) {
  int c = getNextChar();
  assert(c == '/' && "Expected the next character to be a '/'");

  std::string comment;
  result.loc.line = line_;
  result.loc.col = col_;
  c = getNextChar();

  if (c == '*') {
    // Multiline comment. Continue until we find the ending */
    while (true) {
      int next_char = getNextChar();

      if (next_char == EOF) {
        getDiag().Err(getCurrentLoc())
            << "Could not find the end of the last multiline comment.";
        return false;
      } else if (next_char == '*' && input_.peek() == '/') {
        // End of the multiline comment. Consume the next token.
        getNextChar();
        break;
      }

      comment += static_cast<char>(next_char);
    }
  } else if (c != '/') {
    getDiag().Err(result.loc) << "Expected a comment to start with '//'.";
    return false;
  } else {
    // Single line comment. Skip until the end of the line.
    int next_char = getNextChar();
    while (next_char != '\n' && next_char != EOF) {
      comment += static_cast<char>(next_char);
      next_char = getNextChar();
    }
  }

  if (lex_comments_) {
    result.kind = TOK_COMMENT;
    result.chars = comment;
  }
  return true;
}

static const std::unordered_map<std::string, TokenKind> kKeywordMap = {
#define KEYWORD(Kind, Chars) {Chars, Kind},
#include "Tokens.def"
};

bool Lexer::LexKeyword(const std::string &word, Token &result) {
  auto found_keyword = kKeywordMap.find(word);
  if (found_keyword == kKeywordMap.end()) return false;
  result.kind = found_keyword->second;
  result.chars = word;
  return true;
}

bool Lexer::Peek(Token &result) {
  if (!has_lookahead_) {
    bool success = Lex(lookahead_);
    result = lookahead_;
    has_lookahead_ = success;
    return success;
  }
  result = lookahead_;
  return true;
}

}  // namespace qwip
