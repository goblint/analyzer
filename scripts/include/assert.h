#define _ASSERT_H 1
#include <stdio.h>

#define assert(expr)                                                                           \
      ((expr)                                                                                  \
           ? printf("%s:%i: %s: Assertion `%s' passed\n", __FILE__, __LINE__, __func__, #expr) \
           : printf("%s:%i: %s: Assertion `%s' failed\n", __FILE__, __LINE__, __func__, #expr))
