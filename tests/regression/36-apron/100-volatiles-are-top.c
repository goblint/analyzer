// SKIP PARAM: --set ana.activated[+] apron --set ana.apron.domain interval --enable exp.volatiles_are_top
// Test that exp.volatiles_are_top is respected by the relational (apron) analysis.
// volatile and extern variables should be treated as top when read.
#include <goblint.h>

extern int ext;
volatile int vol;

int main(void) {
  ext = 5;
  int x = ext;   // x should be top: ext is extern, exp.volatiles_are_top is on
  __goblint_check(x == 5); // UNKNOWN!

  vol = 3;
  int y = vol;   // y should be top: vol is volatile, exp.volatiles_are_top is on
  __goblint_check(y == 3); // UNKNOWN!

  return 0;
}
