// PARAM: --enable ana.sv-comp.functions --set ana.activated[+] memOutOfBounds --enable ana.int.interval
#include <stdlib.h>
#include <stdio.h>

extern _Bool __VERIFIER_nondet_bool();

int main(void) {
  int foo[4];
  int *bar = malloc(4 * sizeof(int));

  int *p;
  if (__VERIFIER_nondet_bool())
    p = &foo[3];
  else
    p = &bar[3];

  *p = 42; // NOWARN
  return 0;
}
