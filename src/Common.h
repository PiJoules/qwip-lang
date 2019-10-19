#ifndef COMMON_H
#define COMMON_H

#include <cassert>

// In the event NDEBUG is set, it could be possible for an implementationt that
// defines assert to nothing (#define assert(expr) // nothing). Any code that
// would be in this assert would not be run. Additionally, this could also lead
// to `unused variable` warnings. For this case, take control of the definition
// of the assert macro to not be removed completely.
#ifdef NDEBUG
#ifdef assert
#undef assert
#endif
#define assert(expr) (void)(expr);
#endif

#define UNREACHABLE(Msg) assert(false && Msg);

#define __stringify(s) #s
#define STR(s) __stringify(s)

#endif
