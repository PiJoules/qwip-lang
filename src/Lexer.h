#ifndef LEXER_H
#define LEXER_H

#include <fstream>
#include <string>

#include "Common.h"
#include "Diagnostics.h"

namespace qwip {

enum TokenKind {
#define TOKEN(Kind) Kind,
#include "Tokens.def"
};

std::string TokenKindAsString(TokenKind kind);

struct Token {
  TokenKind kind;
  std::string chars;
  SourceLocation loc;

  std::string toString() const {
    std::ostringstream os;
    os << "<" << TokenKindAsString(kind) << " "
       << "chars='" << chars << "' "
       << "loc=" << loc.toString() << " "
       << ">";
    return os.str();
  }
};

class Lexer {
 public:
  Lexer(const std::string &filename) : filename_(filename), input_(filename) {}
  Lexer(const std::string &filename, const Diagnostic &diag)
      : filename_(filename), input_(filename), diag_(diag) {}

  void setLexComments(bool lex_comments = true) {
    lex_comments_ = lex_comments;
  }
  bool getLexComments() const { return lex_comments_; }
  const std::string &getFilename() const { return filename_; }

  bool Lex(Token &result);
  bool Peek(Token &result);

 private:
  bool LexInt(Token &result);
  bool LexString(Token &result);
  bool LexSingleCharToken(char lookahead, Token &result);
  bool LexKeywordOrID(Token &result);
  bool LexKeyword(const std::string &word, Token &result);
  bool LexComment(Token &result);

  int getNextChar() {
    int c = input_.get();
    if (c == EOF) {
      return c;
    } else if (c == '\n') {
      ++line_;
      col_ = 1;
    } else {
      ++col_;
    }
    return c;
  }

  const Diagnostic &getDiag() const { return diag_; }
  SourceLocation getCurrentLoc() const {
    SourceLocation loc;
    loc.line = line_;
    loc.col = col_;
    loc.filename = filename_;
    return loc;
  }

  const std::string filename_;
  std::ifstream input_;
  const Diagnostic diag_;

  bool lex_comments_ = false;
  bool has_lookahead_ = false;
  Token lookahead_;
  unsigned line_ = 1, col_ = 1;
};

}  // namespace qwip

#endif
