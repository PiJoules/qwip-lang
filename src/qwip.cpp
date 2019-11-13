#include <iostream>
#include <string>

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
  ParsedCmdLineArgs cmd_args;
  if (!ParseCommandLineArgs(cmd_args, argc, argv)) return 1;

  Diagnostic diag;
  Lexer lexer(cmd_args.input_file, diag);
  if (cmd_args.dump_tokens) return DumpTokens(lexer) ? 0 : 1;

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
