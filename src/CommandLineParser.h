#ifndef COMMAND_LINE_PARSER_H
#define COMMAND_LINE_PARSER_H

#include <string>
#include <unordered_map>

namespace qwip {

struct ParsedCmdLineArgs {
  std::string input_file;
  std::string output_file = "a.out";

  bool dump_tokens = false;
  bool dump_llvm = false;
  bool lex_comments = false;

  bool FoundInputFile() const { return !input_file.empty(); }
};

bool ParseCommandLineArgs(ParsedCmdLineArgs &args, int argc, char **argv);

}  // namespace qwip

#endif
