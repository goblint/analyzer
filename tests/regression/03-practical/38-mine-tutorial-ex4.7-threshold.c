// PARAM: --enable ana.int.interval --enable ana.sv-comp.functions --enable ana.int.interval_threshold_widening --set ana.int.interval_threshold_widening_constants comparisons
// Copy of 56-witness/27-mine-tutorial-ex4.7 without witness.
#include <goblint.h>
extern _Bool __VERIFIER_nondet_bool();
int main() {
  int x = 0;
  while (__VERIFIER_nondet_bool() == 0) {
    __goblint_check(0 <= x);
    __goblint_check(x <= 40);
    if (__VERIFIER_nondet_bool() == 0) {
      x++;
      if (x > 40)
        x = 0;
    }
  }
  return 0;
}
