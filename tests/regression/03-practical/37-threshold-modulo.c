// PARAM: --enable ana.sv-comp.functions --enable ana.int.interval --enable ana.int.interval_threshold_widening --set ana.int.interval_threshold_widening_constants comparisons --enable exp.no-narrow
#include <goblint.h>

extern _Bool __VERIFIER_nondet_bool();

int main() {
  int x = 0;

  while (__VERIFIER_nondet_bool()) {
    x = (x + 1) % 10;
  }

  __goblint_check(x >= 0);
  __goblint_check(x <= 9);

  int y = 0;

  while (__VERIFIER_nondet_bool()) {
    y = (y - 1) % 10;
  }

  __goblint_check(y >= -9);

  // Second example from #2135: the counter is reset by a modulo guard rather
  // than by a modulo assignment.
  int z = 1;

  while (__VERIFIER_nondet_bool()) {
    if (z % 10 == 0)
      z = 1;
    else
      z++;
  }

  __goblint_check(z <= 10);

  return 0;
}
