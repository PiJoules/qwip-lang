#ifndef COMMAND_LINE_PARSER_H
#define COMMAND_LINE_PARSER_H

#include <cassert>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

#define DEFINE_OPT_BOOL_FLAG(Args, Name, Flag)         \
  bool Assign##Name(CommandLineParser<Args> &parser) { \
    parser.getArgs().Flag = true;                      \
    return true;                                       \
  }

namespace qwip {

/**
 * Helper class for parsing command line arguments.
 */
template <class Args>
class CommandLineParser {
 public:
  using ArgHandler = bool (*)(CommandLineParser<Args> &);
  using PositionalHandlers = std::vector<ArgHandler>;
  using OptionalHandlers = std::unordered_map<std::string, ArgHandler>;

  CommandLineParser(const PositionalHandlers &pos_handlers,
                    const OptionalHandlers &opt_handlers)
      : pos_handlers_(pos_handlers), opt_handlers_(opt_handlers) {}

  bool Parse(Args &args, int argc, char **argv) {
    return Parse(args, static_cast<unsigned>(argc), argv);
  }

  bool Parse(Args &args, unsigned argc, char **argv) {
    ResetState();
    args_ = &args;
    argc_ = argc;
    argv_ = argv;

    while (!DidReadAllArgs()) {
      std::string arg(getCurrentArg());
      current_flag_ = getCurrentArg();

      auto found_callback = opt_handlers_.find(arg);
      if (found_callback == opt_handlers_.end()) {
        // Could be a flag.
        if (arg.find("-") == 0) {
          std::cerr << "Unknown flag: " << arg << "\n";
          return false;
        }

        // Parsing positional arguments.
        if (current_positional_flag_ >= pos_handlers_.size()) {
          std::cerr
              << "Too many positional arguments provided. Expected at most "
              << pos_handlers_.size() << ".\n";
          return false;
        }

        if (!(pos_handlers_.at(current_positional_flag_)(*this))) return false;

        ++current_positional_flag_;
        continue;
      }

      // Found an optional flag. Call its handler.
      // The first argument an optional flag should see is what's passed as it's
      // argument.
      Advance();
      if (!((found_callback->second)(*this))) return false;
    }

    if (current_positional_flag_ < pos_handlers_.size()) {
      std::cerr << "Not enough positional arguments provided. Found "
                << current_positional_flag_ << " but expected "
                << pos_handlers_.size() << ".\n";
      return false;
    }
    return true;
  }

  bool getCurrentIdx() const { return current_arg_; }
  const char *getCurrentArg() const { return argv_[current_arg_]; }
  const char *getCurrentFlag() const { return current_flag_; }
  bool DidReadAllArgs() const { return current_arg_ >= argc_; }
  Args &getArgs() { return *args_; }
  unsigned getArgc() const { return argc_; }
  const char **getArgv() const { return argv_; }

  void Advance(unsigned i = 1) {
    assert(!DidReadAllArgs() &&
           "Already reached end of arguments. Be sure to check "
           "DidReadAllArgs() before calling Advance().");
    current_arg_ += i;
  }

  const char *getCurrentArgAndAdvance() {
    const char *arg = getCurrentArg();
    Advance();
    return arg;
  }

 private:
  void ResetState() {
    current_arg_ = 1;  // Always start on the 2nd argument
    current_flag_ = nullptr;
    current_positional_flag_ = 0;
  }

  const PositionalHandlers &pos_handlers_;
  const OptionalHandlers &opt_handlers_;
  Args *args_;
  unsigned argc_;
  char **argv_;

  // These need to be reset before every Parse() call.
  unsigned current_arg_;
  const char *current_flag_;
  unsigned current_positional_flag_;
};

}  // namespace qwip

#endif
