// PARAM: --enable ana.int.def_exc --enable ana.int.interval
// ldv-benchmarks: u__linux-concurrency_safety__drivers---net---ethernet---amd---pcnet32.ko.c
#include <goblint.h>

int main() {
  int debug_value = -1;

  if ((unsigned int)debug_value > 31U)
    __goblint_check(1); // reachable
  else
    __goblint_check(1); // NOWARN (unreachable)

  return 0;
}