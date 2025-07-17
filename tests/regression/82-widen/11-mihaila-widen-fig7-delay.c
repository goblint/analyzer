// PARAM: --disable ana.int.def_exc --enable ana.int.interval --enable ana.sv-comp.functions --set ana.widen.delay.local 1
// From "Widening as Abstract Domain", Fig. 7: https://doi.org/10.1007/978-3-642-38088-4_12
// Delay 1:
// 0. {x -> [0,0], y -> [0,0]}
// 1. {x -> [0,4], y -> [0,1]} (delay 0 would widen already here)
// 2. {x -> [0,+inf], y -> [0,1]}
// narrow: {x -> [0,103], y -> [0,1]}
#include <goblint.h>
extern _Bool __VERIFIER_nondet_bool();

int main() {
  int x = 0;
  int y = 0;
  while (x < 100) {
    __goblint_check(0 <= y);
    __goblint_check(y <= 1);
    if (__VERIFIER_nondet_bool())
      y = 1;
    x = x + 4;
  }
  __goblint_check(0 <= y);
  __goblint_check(y <= 1);
  return 0;
}
