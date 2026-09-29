// PARAM: --enable ana.autotune.enabled --set ana.path_sens[+] base --set ana.activated[+] abortUnless
// Reduced with creduce from sv-benchmarks/c/nla-digbench-scaling/egcd-ll_valuebound5.c

#include <assert.h>
extern void abort(void);
void reach_error() { assert(0); }
void __VERIFIER_assert(int cond) { if(!(cond)) { ERROR: {reach_error();abort();} } }

long c;
main() {
  int d;
  long a, b;
  while (1) {
    if (!b)
      __VERIFIER_assert(a == c);
    __VERIFIER_assert(b * d);
    b = a;
    c = 1;
  }
}
