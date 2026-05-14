// SKIP PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow
// assume_none NOCRASH

#include <goblint.h>

int main(void) {
  int a;
  int b;

  __goblint_assume(2 * a + 2 * b + 2 < 4);
  __goblint_assume(a + b + 1 < 2);

  return 0;
}
