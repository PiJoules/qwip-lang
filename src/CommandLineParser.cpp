#include "CommandLineParser.h"

#include <cassert>
#include <iostream>
#include <string>

namespace qwip {

namespace {

// The class is a helper for parsing arguments and isn't meant to be used by
// anything outside this project. Ideally, the interface to use this class will
// be through free functions that construct this and call Parse() only once.
class CommandLineParser {
 public:
  CommandLineParser(ParsedCmdLineArgs &args, unsigned argc, char **argv)
      : args_(args), argc_(argc), argv_(argv) {}

  bool Parse();

 private:
  void ResetState() {
    current_arg_ = 1;
    current_flag_ = nullptr;
  }

  bool getCurrentIdx() const { return current_arg_; }
  const char *getCurrentArg() const { return argv_[current_arg_]; }
  const char *getCurrentFlag() const { return current_flag_; }
  bool DidReadAllArgs() const { return current_arg_ >= argc_; }

  void Advance(unsigned i = 1) {
    assert(!DidReadAllArgs() && "Already reached end of arguments");
    current_arg_ += i;
  }

  typedef bool (CommandLineParser::*ArgCallback)();
  static const std::unordered_map<std::string, ArgCallback>
      kCommandLineCallbacks;

  bool AssignDumpTokens();
  bool AssignDumpLLVM();
  bool AssignOutputFile();

  // These are only set once on construction.
  ParsedCmdLineArgs &args_;
  unsigned argc_;
  char **argv_;

  // These need to be reset before every Parse() call.
  unsigned current_arg_;
  const char *current_flag_;
};

bool CommandLineParser::Parse() {
  ResetState();

  while (!DidReadAllArgs()) {
    std::string arg(getCurrentArg());
    auto found_callback = kCommandLineCallbacks.find(arg);
    if (found_callback == kCommandLineCallbacks.end()) {
      if (args_.FoundInputFile()) {
        std::cerr << "Already found " << args_.input_file
                  << " as an input, but also found " << arg
                  << " as an input. Only one input can be passed at a time.\n";
        return false;
      }

      args_.input_file = arg;
      Advance();
      continue;
    }

    current_flag_ = getCurrentArg();
    if (!(this->*found_callback->second)()) return false;
  }
  return true;
}

const std::unordered_map<std::string, CommandLineParser::ArgCallback>
    CommandLineParser::kCommandLineCallbacks = {
        {"--dump-tokens", &CommandLineParser::AssignDumpTokens},
        {"--dump-llvm", &CommandLineParser::AssignDumpLLVM},
        {"-o", &CommandLineParser::AssignOutputFile},
};

bool CommandLineParser::AssignDumpTokens() {
  Advance();
  args_.dump_tokens = true;
  return true;
}

bool CommandLineParser::AssignDumpLLVM() {
  Advance();
  args_.dump_llvm = true;
  return true;
}

bool CommandLineParser::AssignOutputFile() {
  Advance();
  if (DidReadAllArgs()) {
    std::cerr << "No argument provided for " << getCurrentFlag() << "\n";
    return false;
  }
  args_.output_file = getCurrentArg();
  Advance();
  return true;
}

}  // namespace

bool ParseCommandLineArgs(ParsedCmdLineArgs &args, int argc, char **argv) {
  return CommandLineParser(args, static_cast<unsigned>(argc), argv).Parse();
}

}  // namespace qwip
