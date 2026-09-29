// PARAM: --conf conf/svcomp26/common.json --conf conf/svcomp26/verify.json --conf conf/svcomp26/level05.json --set exp.architecture 32bit --set ana.specification "CHECK( init(main()), LTL(G ! overflow) )"
// Reduced with creduce from sv-benchmarks/c/nla-digbench-scaling/lcm1_valuebound5.c

// #include <assert.h>
// extern void abort(void);
// void reach_error() { assert(0); }
// void __VERIFIER_assert(int cond) { if(!(cond)) { ERROR: {reach_error();abort();} } }

unsigned d, e;
c(f) {
  if (!f)
    abort();
}
main() {
  unsigned a, b;
  c(a <= 5);
  c(b && b <= 5);
  d = a;
  e = b;
  while (1) {
    if (!(d != e))
      break;
    while (1) {
      if (!(d > e))
        break;
      d = d - e;
    }
    while (1) {
      __VERIFIER_assert(a * b);
      if (!(d < e))
        break;
      e = e - d;
    }
  }
}
