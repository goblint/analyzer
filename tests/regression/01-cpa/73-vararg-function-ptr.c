#include<stdio.h>
#include<stdarg.h>

typedef void (*fnct_ptr)(void);
typedef int (*foo_t)(int,...);

int foo (int x, ...) {
  return x;
}

int main () {
  fnct_ptr f;
  foo_t k;
  f = (fnct_ptr) &foo;
  k = (foo_t) f;

  k(1); // NOWARN
  k(1, 2); // NOWARN

  ((foo_t)f) (1); // NOWARN
  ((foo_t)f) (1, 2); // NOWARN

  // Calls with too few arguments
  f(); // WARN
  ((fnct_ptr) k)(); // WARN

  return 0;
}
