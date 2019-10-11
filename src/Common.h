#ifndef COMMON_H
#define COMMON_H

#include <cassert>

// In the event NDEBUG is set, asserts and what gets passed to assert is removed
// entirely which could mean that functions called in those asserts also don't
// get called. We want to still run those operations and only get rid of the
// checking part, so we will use this macro instead.
#define CHECK(Expr, Msg)    \
  {                         \
    bool __result = Expr;   \
    (void)__result;         \
    assert(__result&& Msg); \
  }

#define UNREACHABLE(Msg) CHECK(false, Msg);

#define __stringify(s) #s
#define STR(s) __stringify(s)

#endif
