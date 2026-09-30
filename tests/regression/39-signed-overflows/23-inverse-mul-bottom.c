// PARAM: --enable ana.int.interval --enable ana.int.bitfield --set ana.activated[+] apron --set ana.apron.domain polyhedra --set exp.unrolling-factor 2
// Reduced with creduce from sv-benchmarks/c/nla-digbench-scaling/lcm1_valuebound5.c

unsigned d, e;
main() {
  unsigned a, b;
  __goblint_assume(a <= 5);
  __goblint_assume(b && b <= 5);
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
      __goblint_assume(a * b); // NOCRASH
      if (!(d < e))
        break;
      e = e - d;
    }
  }
}
