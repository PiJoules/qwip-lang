#include <iostream>
#include <string>
#include <vector>

#include "CommandLineParser.h"
#include "Compiler.h"
#include "Lexer.h"
#include "Parser.h"

/**
 *  global_var : i32;
 *  global_var_assigned : i32 = 123;
 *
 *  main: (argc: i32, argv: i8**) -> i32 {
 *    ret 0;
 *  }
 */

using namespace qwip;

namespace {

struct QwipArgs {
  std::string input_file;
  std::string output_file = "a.out";

  bool dump_tokens = false;
  bool dump_llvm = false;
  bool lex_comments = false;

  bool FoundInputFile() const { return !input_file.empty(); }
};

DEFINE_OPT_BOOL_FLAG(QwipArgs, DumpTokens, dump_tokens)
DEFINE_OPT_BOOL_FLAG(QwipArgs, DumpLLVM, dump_llvm)
DEFINE_OPT_BOOL_FLAG(QwipArgs, LexComments, lex_comments)

bool AssignOutputFile(CommandLineParser<QwipArgs> &parser) {
  auto &args = parser.getArgs();
  if (parser.DidReadAllArgs()) {
    std::cerr << "No output file provided.\n";
    return false;
  }
  args.output_file = parser.getCurrentArgAndAdvance();
  return true;
}

bool AssignInputFile(CommandLineParser<QwipArgs> &parser) {
  auto &args = parser.getArgs();
  assert(!args.FoundInputFile() && "Already found an input file.");
  args.input_file = parser.getCurrentArgAndAdvance();
  return true;
}

const CommandLineParser<QwipArgs>::OptionalHandlers kQwipOptHandlers = {
    {"--dump-tokens", &AssignDumpTokens},
    {"--dump-llvm", &AssignDumpLLVM},
    {"--lex-comments", &AssignLexComments},
    {"-o", &AssignOutputFile},
};

const CommandLineParser<QwipArgs>::PositionalHandlers kQwipPosHandlers = {
    &AssignInputFile,
};

bool DumpTokens(Lexer &lexer) {
  Token tok;
  do {
    if (!lexer.Lex(tok)) return false;
    std::cerr << tok.toString() << "\n";
  } while (tok.kind != TOK_EOF);
  return true;
}

}  // namespace

int main(int argc, char **argv) {
  QwipArgs cmd_args;
  if (!CommandLineParser(kQwipPosHandlers, kQwipOptHandlers)
           .Parse(cmd_args, argc, argv))
    return 1;

  Diagnostic diag;
  Lexer lexer(cmd_args.input_file, diag);
  if (cmd_args.dump_tokens) {
    lexer.setLexComments(cmd_args.lex_comments);
    return DumpTokens(lexer) ? 0 : 1;
  }

  Parser parser(lexer, diag);

  std::unique_ptr<Module> module;
  if (!parser.Parse(module)) return 1;

  Compiler compiler(diag);
  if (!compiler.CompileModule(*module)) return 1;

  if (cmd_args.dump_llvm) {
    compiler.getLLVMModule().print(llvm::errs(), nullptr);
    return 0;
  }

  if (!compiler.SaveToExecutable(cmd_args.input_file, cmd_args.output_file)) {
    // This line is excluded because there are a few ways that a failure can be
    // reached here and they can't easily be tested, so we just skip this branch
    // for now since the function should gracefully handle the error and spit
    // some message.
    return 1;  // LCOV_EXCL_LINE
  }

  return 0;
}
