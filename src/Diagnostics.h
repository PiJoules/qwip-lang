#ifndef DIAGNOSTICS_H
#define DIAGNOSTICS_H

#include <iostream>
#include <sstream>
#include <string>

namespace qwip {

struct SourceLocation {
  unsigned line = 0;
  unsigned col = 0;
  std::string filename;

  bool isValid() const { return line > 0 && col > 0; }
  std::string toString() const {
    std::ostringstream os;
    if (filename.empty())
      os << "<unknown file>";
    else
      os << filename;
    os << ":" << line << ":" << col;
    return os.str();
  }
};

/**
 * // Create once with any configuration.
 * Diagnostic diag_(std::cerr);
 *
 * // Then print the errors/warnings on the fly.
 * diag_.Err(loc) << "Found " << x << " when expected " << y;
 */

enum DiagKind {
  DIAG_ERR,
  DIAG_WARN,
  DIAG_NOTE,
};

class Message {
 public:
  Message(std::ostream &out, const SourceLocation &loc, DiagKind kind)
      : out_(out) {
    out_ << loc.toString() << ": ";
    switch (kind) {
      case DIAG_ERR:
        out_ << "error: ";
        break;
      case DIAG_WARN:
        out_ << "warning: ";
        break;
      case DIAG_NOTE:
        out_ << "note: ";
        break;
    }
  }

  template <typename T>
  Message &operator<<(const T &t) = delete;

  Message &operator<<(const char *s) {
    out_ << s;
    return *this;
  }
  Message &operator<<(const std::string &s) {
    out_ << s;
    return *this;
  }
  Message &operator<<(char c) {
    out_ << c;
    return *this;
  }
  Message &operator<<(size_t size) {
    out_ << size;
    return *this;
  }

  ~Message() {
    // If RVO is not enabled, this may be printed on returning in the builder,
    // but this isn't so bad.
    out_ << "\n";
  }

 private:
  std::ostream &out_;
};

class Diagnostic {
 public:
  Diagnostic(std::ostream &out) : out_(out) {}
  Diagnostic() : Diagnostic(std::cerr) {}
  Message Err(const SourceLocation &loc) const {
    return Message(out_, loc, DIAG_ERR);
  }
  Message Warn(const SourceLocation &loc) const {
    return Message(out_, loc, DIAG_WARN);
  }
  Message Note(const SourceLocation &loc) const {
    return Message(out_, loc, DIAG_NOTE);
  }

 private:
  std::ostream &out_;
};

}  // namespace qwip

#endif
