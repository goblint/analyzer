// PARAM: --enable ana.autotune.enabled --set ana.path_sens[+] base --set ana.activated[+] abortUnless
// Reduced with creduce from sv-benchmarks/c/nla-digbench-scaling/egcd-ll_valuebound5.c

long c;
main() {
  int d;
  long a, b;
  while (1) {
    if (!b)
      __goblint_assert(a == c); // UNKNOWN
    __goblint_assert(b * d); // UNKNOWN NOCRASH
    b = a;
    c = 1;
  }
}
