#include <iostream>
#include <string>

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

struct CommandLineArgs {
  std::string input_file;
  std::string output_file = "a.out";

  bool dump_tokens = false;
  bool dump_llvm = false;
};

bool ParseCommandLine(int argc, char **argv, CommandLineArgs &cmd_args) {
  bool found_input_file = false;
  for (int i = 1; i < argc; ++i) {
    std::string arg(argv[i]);
    if (arg == "--dump-tokens") {
      cmd_args.dump_tokens = true;
    } else if (arg == "--dump-llvm") {
      cmd_args.dump_llvm = true;
    } else if (arg == "-o") {
      ++i;
      if (i >= argc) {
        std::cerr << "No argument provided for -o\n";
        return false;
      }
      arg = argv[i];
      cmd_args.output_file = arg;
    } else if (!found_input_file) {
      cmd_args.input_file = arg;
      found_input_file = true;
    } else {
      std::cerr << "Already found " << cmd_args.input_file
                << " as an input, but also found " << arg
                << " as an input. Only one input can be passed at a time.\n";
      return false;
    }
  }

  if (!found_input_file) {
    std::cerr << "Missing input file\n";
    return false;
  }

  return true;
}

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
  CommandLineArgs cmd_args;
  if (!ParseCommandLine(argc, argv, cmd_args)) return 1;

  Diagnostic diag;
  Lexer lexer(cmd_args.input_file, diag);
  if (cmd_args.dump_tokens) return DumpTokens(lexer) ? 0 : 1;

  Parser parser(lexer, diag);

  std::unique_ptr<Module> module;
  if (!parser.ParseModule(module)) return 1;

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
