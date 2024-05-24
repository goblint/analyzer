// SKIP PARAM: --enable ana.sv-comp.functions --enable ana.autotune.enabled --set ana.autotune.activated[+] octagon
// Extracted from: nla-digbench-scaling/sqrt1-ll_unwindbound5.c
#include <goblint.h>
extern int __VERIFIER_nondet_int(void);

int main() {
  int n;
  long long s;
  n = __VERIFIER_nondet_int();

  if (!(s <= n)) {
    __goblint_check(s > n);
  } else {
    __goblint_check(s <= 2147483647);
  }

  return 0;
}
