#include <vector>

#include "CommandLineParser.h"
#include "Diagnostics.h"
#include "Lexer.h"
#include "Parser.h"

#define CATCH_CONFIG_MAIN
#include "catch.hpp"

using namespace qwip;

namespace {

struct DummyArgs {};

class Arguments {
 public:
  Arguments(std::initializer_list<char const *> strs) {
    for (const auto &str : strs) {
      auto str_len = strlen(str);
      vec_.push_back(new char[str_len + 1]);
      strncpy(vec_.back(), str, str_len);
      vec_.back()[str_len] = 0;
    }
  }

  ~Arguments() {
    for (char *arg : vec_) delete[] arg;
  }

  char **get() { return vec_.data(); }

 private:
  std::vector<char *> vec_;
};

bool DummyHandlerSuccess(CommandLineParser<DummyArgs> &parser) {
  // Do nothing.
  parser.Advance();
  return true;
}

bool DummyHandlerFail(CommandLineParser<DummyArgs> &parser) {
  // Do nothing.
  parser.Advance();
  return false;
}

const CommandLineParser<DummyArgs>::PositionalHandlers kEmptyPosHandlers;
const CommandLineParser<DummyArgs>::OptionalHandlers kEmptyOptHandlers;

TEST_CASE("Invalid flag", "[CommandLineParser]") {
  CommandLineParser<DummyArgs> parser(kEmptyPosHandlers, kEmptyOptHandlers);
  DummyArgs args;
  Arguments argv({"exe", "-bad-flag"});
  REQUIRE_FALSE(parser.Parse(args, 2, argv.get()));
}

TEST_CASE("Too many positional arguments", "[CommandLineParser]") {
  CommandLineParser<DummyArgs> parser(kEmptyPosHandlers, kEmptyOptHandlers);
  DummyArgs args;
  Arguments argv({"exe", "arg"});
  REQUIRE_FALSE(parser.Parse(args, 2, argv.get()));
}

TEST_CASE("Too few positional arguments", "[CommandLineParser]") {
  CommandLineParser<DummyArgs>::PositionalHandlers pos_handlers = {
      &DummyHandlerSuccess,
  };
  CommandLineParser<DummyArgs> parser(pos_handlers, kEmptyOptHandlers);
  DummyArgs args;
  Arguments argv({"exe"});
  REQUIRE_FALSE(parser.Parse(args, 1, argv.get()));
}

TEST_CASE("Positional handler fail", "[CommandLineParser]") {
  CommandLineParser<DummyArgs>::PositionalHandlers pos_handlers = {
      &DummyHandlerFail,
  };
  CommandLineParser<DummyArgs> parser(pos_handlers, kEmptyOptHandlers);
  DummyArgs args;
  Arguments argv({"exe", "arg"});
  REQUIRE_FALSE(parser.Parse(args, 2, argv.get()));
}

TEST_CASE("Positional handler success", "[CommandLineParser]") {
  CommandLineParser<DummyArgs>::PositionalHandlers pos_handlers = {
      &DummyHandlerSuccess,
  };
  CommandLineParser<DummyArgs> parser(pos_handlers, kEmptyOptHandlers);
  DummyArgs args;
  Arguments argv({"exe", "arg"});
  REQUIRE(parser.Parse(args, 2, argv.get()));
}

TEST_CASE("Optional handler fail", "[CommandLineParser]") {
  CommandLineParser<DummyArgs>::OptionalHandlers opt_handlers = {
      {"-arg", &DummyHandlerFail},
  };
  CommandLineParser<DummyArgs> parser(kEmptyPosHandlers, opt_handlers);
  DummyArgs args;
  Arguments argv({"exe", "-arg"});
  REQUIRE_FALSE(parser.Parse(args, 2, argv.get()));
}

TEST_CASE("Test getCurrentFlag()", "[CommandLineParser]") {
  auto check_flag = [](CommandLineParser<DummyArgs> &parser) {
    REQUIRE(strcmp(parser.getCurrentFlag(), "-arg") == 0);
    parser.Advance();
    return true;
  };
  CommandLineParser<DummyArgs>::OptionalHandlers opt_handlers = {
      {"-arg", check_flag},
  };
  CommandLineParser<DummyArgs> parser(kEmptyPosHandlers, opt_handlers);
  DummyArgs args;
  Arguments argv({"exe", "-arg"});
  REQUIRE(parser.Parse(args, 2, argv.get()));
}

TEST_CASE("Invalid SourceLocation", "[SourceLocation]") {
  SourceLocation loc;
  REQUIRE((loc.toString().find("<unknown file>") == 0 &&
           "Expected the source location to be unknown"));
}

TEST_CASE("Note message", "[Diagnostic]") {
  std::stringstream s;
  Diagnostic diag(s);
  SourceLocation loc;
  diag.Note(loc);
  REQUIRE(s.str() == "<unknown file>:0:0: note: \n");
}

TEST_CASE("Diagnostic char printing", "[Diagnostic]") {
  std::stringstream s;
  Diagnostic diag(s);
  SourceLocation loc;
  diag.Note(loc) << 'c';
  REQUIRE(s.str() == "<unknown file>:0:0: note: c\n");
}

TEST_CASE("TokenKind to string", "[Lexer]") {
  REQUIRE(TokenKindAsString(TOK_ID) == "TOK_ID");
}

TEST_CASE("String type comparisons", "[QwipTypes]") {
  StrType str_type(5);
  StrType str_type2(5);
  REQUIRE(str_type == str_type2);
  std::unique_ptr<Type> char_type(new IntType(kNumCharBits));
  ArrayType arr_type(char_type, 5);

  {
    std::unique_ptr<Type> char_type(new IntType(kNumCharBits));
    ArrayType arr_type(char_type, 5);
    REQUIRE(str_type == arr_type);
    REQUIRE(arr_type == str_type);
  }
  {
    std::unique_ptr<Type> char_type(new IntType(kNumCharBits));
    ArrayType arr_type(char_type, 6);
    REQUIRE(str_type != arr_type);
  }
  {
    std::unique_ptr<Type> char_type(new IntType(kNumCharBits));
    std::unique_ptr<Type> nested_arr_type(new ArrayType(char_type, 6));
    ArrayType arr_type(nested_arr_type, 6);
    REQUIRE(str_type != arr_type);
  }
  {
    // A pointer type can be equivalent to a string type as long as it is a char
    // pointer.
    std::unique_ptr<Type> char_type(new IntType(kNumCharBits));
    PtrType ptr_type(char_type);
    REQUIRE(str_type == ptr_type);
    REQUIRE(arr_type == ptr_type);
    REQUIRE(ptr_type == str_type);
    REQUIRE(ptr_type == arr_type);
  }
  {
    IntType char_type(kNumCharBits);
    REQUIRE(str_type != char_type);
  }
}

}  // namespace
