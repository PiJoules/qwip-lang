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

bool AssignDumpTokens(CommandLineParser<QwipArgs> &parser) {
  auto &args = parser.getArgs();
  parser.Advance();
  args.dump_tokens = true;
  return true;
}

bool AssignDumpLLVM(CommandLineParser<QwipArgs> &parser) {
  auto &args = parser.getArgs();
  parser.Advance();
  args.dump_llvm = true;
  return true;
}

bool AssignLexComments(CommandLineParser<QwipArgs> &parser) {
  auto &args = parser.getArgs();
  parser.Advance();
  args.lex_comments = true;
  return true;
}

bool AssignOutputFile(CommandLineParser<QwipArgs> &parser) {
  auto &args = parser.getArgs();
  parser.Advance();
  if (parser.DidReadAllArgs()) {
    std::cerr << "No argument provided for " << parser.getCurrentFlag() << "\n";
    return false;
  }
  args.output_file = parser.getCurrentArg();
  parser.Advance();
  return true;
}

bool AssignInputFile(CommandLineParser<QwipArgs> &parser) {
  auto &args = parser.getArgs();
  if (args.FoundInputFile()) {
    std::cerr << "Already found " << args.input_file
              << " as an input, but also found " << parser.getCurrentArg()
              << " as an input. Only one input can be passed at a time.\n";
    return false;
  }

  args.input_file = parser.getCurrentArg();
  parser.Advance();
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
    if (!lexer.Lex(tok)) {
      std::cerr << "Lexer error\n";
      return false;
    }
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

  if (!compiler.SaveToExecutable(cmd_args.input_file, cmd_args.output_file))
    return 1;

  return 0;
}
