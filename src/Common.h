#ifndef COMMON_H
#define COMMON_H

#include <cassert>

#define UNREACHABLE(Msg) assert(false && Msg);

#define __stringify(s) #s
#define STR(s) __stringify(s)

// Silence warnings from LLVM code.
#if defined(__clang__)
#define __SILENCE_LLVM_WARNINGS_START \
  _Pragma("clang diagnostic push")    \
      _Pragma("clang diagnostic ignored \"-Wconversion\"")
#elif defined(__GNUC__)
#define __SILENCE_LLVM_WARNINGS_START \
  _Pragma("GCC diagnostic push")      \
      _Pragma("GCC diagnostic ignored \"-Wconversion\"")
#endif

#if defined(__clang__)
#define __SILENCE_LLVM_WARNINGS_END _Pragma("clang diagnostic pop")
#elif defined(__GNUC__)
#define __SILENCE_LLVM_WARNINGS_END _Pragma("GCC diagnostic pop")
#endif

#endif
