#include <goblint.h>
extern int __VERIFIER_nondet_int();

int main() {
  int i;
  i = __VERIFIER_nondet_int();

  if (i < 100)
    __goblint_check(1);

  if (50 < i && i < 100)
    __goblint_check(1);

  if (i == 42 || i == 5 || i == 101)
    __goblint_check(1);
  return 0;
}
