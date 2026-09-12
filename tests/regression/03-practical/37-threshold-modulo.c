// PARAM: --enable ana.int.interval --enable ana.int.interval_threshold_widening --set ana.int.interval_threshold_widening_constants comparisons --enable exp.no-narrow
#include <goblint.h>

int nondet();

int main() {
  int x = 0;

  while (nondet()) {
    x = (x + 1) % 10;
  }

  __goblint_check(x >= 0);
  __goblint_check(x <= 9);

  int y = 0;

  while (nondet()) {
    y = (y - 1) % 10;
  }

  __goblint_check(y >= -9);

  return 0;
}
