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

bool DummyPosHandlerFail(CommandLineParser<DummyArgs> &parser) {
  // Do nothing.
  return false;
}

bool DummyOptHandlerFail(CommandLineParser<DummyArgs> &parser) {
  // Do nothing.
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
      &DummyPosHandlerFail,
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
      {"-arg", &DummyOptHandlerFail},
  };
  CommandLineParser<DummyArgs> parser(kEmptyPosHandlers, opt_handlers);
  DummyArgs args;
  Arguments argv({"exe", "-arg"});
  REQUIRE_FALSE(parser.Parse(args, 2, argv.get()));
}

TEST_CASE("Test getCurrentFlag()", "[CommandLineParser]") {
  auto check_flag = [](CommandLineParser<DummyArgs> &parser) {
    REQUIRE(strcmp(parser.getCurrentFlag(), "-arg") == 0);
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

    auto str_clone = str_type.Clone();
    REQUIRE(*str_clone == str_type);
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

TEST_CASE("Dump type", "[QwipTypes]") {
  {
    IntType type(10);
    std::stringstream ss;
    type.Dump(ss);
    REQUIRE(ss.str() == "i10");
  }

  {
    std::unique_ptr<Type> int_type(new IntType(10));
    PtrType ptr_type(int_type);
    std::stringstream ss;
    ptr_type.Dump(ss);
    REQUIRE(ss.str() == "i10*");
  }

  {
    std::unique_ptr<Type> int_type(new IntType(10));
    ArrayType arr_type(int_type, 2);
    std::stringstream ss;
    arr_type.Dump(ss);
    REQUIRE(ss.str() == "i10[2]");
  }

  {
    std::unique_ptr<Type> void_type(new VoidType);
    std::unique_ptr<Type> int_type(new IntType(10));
    std::unique_ptr<Type> int_type2(new IntType(8));
    std::vector<std::unique_ptr<Type>> args;
    args.push_back(std::move(int_type));
    args.push_back(std::move(int_type2));
    FuncType func(void_type, args, /*isvararg=*/false);
    std::stringstream ss;
    func.Dump(ss);
    REQUIRE(ss.str() == "(i10, i8) -> void");
  }

  {
    std::unique_ptr<Type> int_type(new IntType(10));
    std::unique_ptr<Type> int_type2(new IntType(8));
    std::vector<std::unique_ptr<Type>> types;
    types.push_back(std::move(int_type));
    types.push_back(std::move(int_type2));
    std::unordered_map<std::string, size_t> members = {
        {"a", 0},
        {"b", 1},
    };
    StructType struct_type(types, members);
    std::stringstream ss;
    struct_type.Dump(ss);
    REQUIRE(ss.str() == "{i10, i8}");
  }

  {
    std::vector<std::string> vals = {"a", "b"};
    EnumType enum_type(vals);
    std::stringstream ss;
    enum_type.Dump(ss);
    REQUIRE(ss.str() == "{a, b}");
  }
}

TEST_CASE("Unknown enum member", "[QwipTypes]") {
  std::vector<std::string> vals = {"a", "b"};
  EnumType enum_type(vals);
  size_t value;
  REQUIRE_FALSE(enum_type.getValue("c", value));
}

TEST_CASE("Struct comparisons", "[QwipTypes]") {
  std::unique_ptr<Type> int_type(new IntType(10));
  std::unique_ptr<Type> int_type2(new IntType(8));
  std::vector<std::unique_ptr<Type>> types;
  types.push_back(std::move(int_type));
  types.push_back(std::move(int_type2));
  std::unordered_map<std::string, size_t> members = {
      {"a", 0},
      {"b", 1},
  };
  StructType struct_type(types, members);
  REQUIRE(struct_type == struct_type);
}

TEST_CASE("Function checks", "[QwipTypes]") {
  std::unique_ptr<Type> void_type(new VoidType);
  std::unique_ptr<Type> int_type(new IntType(10));
  std::unique_ptr<Type> int_type2(new IntType(8));
  std::vector<std::unique_ptr<Type>> args;
  args.push_back(std::move(int_type));
  args.push_back(std::move(int_type2));
  FuncType func(void_type, args, /*isvararg=*/false);
  REQUIRE(func == func);
}

TEST_CASE("Subscript type", "[Subscript]") {
  SourceLocation loc;
  IntType int_type(10);
  std::unique_ptr<Expr> val(new Int(loc, 1, /*num_bits=*/10));
  std::vector<std::unique_ptr<Expr>> vals;
  vals.push_back(std::move(val));
  std::unique_ptr<Expr> arr_base(new Array(loc, vals));
  std::unique_ptr<Expr> idx(new Int(loc, 1));
  Subscript subscript(arr_base, idx);
  REQUIRE(*subscript.getType() == int_type);
}

TEST_CASE("Call type", "[Call]") {
  SourceLocation loc;
  IntType int_type(10);
  std::vector<std::unique_ptr<Type>> args;
  std::unique_ptr<Type> ret_type(new IntType(10));
  std::unique_ptr<Type> func_type(
      new FuncType(ret_type, args, /*isvararg=*/false));
  std::unique_ptr<Expr> caller(new ID(loc, "func", func_type));
  Call call(caller);
  REQUIRE(*call.getType() == int_type);
}

TEST_CASE("BinOp type", "[BinOp]") {
  SourceLocation loc;

  for (auto op = FirstComparisonOp; op < LastComparisonOp; ++op) {
    std::unique_ptr<Expr> lhs(new Int(loc, 1, /*num_bits=*/10));
    std::unique_ptr<Expr> rhs(new Int(loc, 1, /*num_bits=*/10));
    BinOp binop(lhs, rhs, static_cast<BinOpCode>(op));
    REQUIRE(*binop.getType() == IntType(32));
  }

  {
    std::unique_ptr<Expr> lhs(new Int(loc, 1, /*num_bits=*/10));
    std::unique_ptr<Expr> rhs(new Int(loc, 1, /*num_bits=*/10));
    BinOp binop(lhs, rhs, BINOP_ADD);
    REQUIRE(*binop.getType() == IntType(10));
  }

  {
    std::unique_ptr<Expr> lhs(new Int(loc, 1, /*num_bits=*/10));
    std::unique_ptr<Expr> rhs(new Int(loc, 1, /*num_bits=*/10));
    BinOp binop(lhs, rhs, BINOP_SUB);
    REQUIRE(*binop.getType() == IntType(10));
  }
}

}  // namespace
