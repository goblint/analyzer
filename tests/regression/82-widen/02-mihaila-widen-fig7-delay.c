// PARAM: --disable ana.int.def_exc --enable ana.int.interval --enable ana.sv-comp.functions --set ana.widen.delay 3
// From "Widening as Abstract Domain", Fig. 7: https://doi.org/10.1007/978-3-642-38088-4_12
// They claim delay 2 (https://bytebucket.org/mihaila/bindead/wiki/resources/widening-talk.pdf), we need 3 for some reason. Why?
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
