// PARAM: --set ana.activated[+] affeq --set sem.int.signed_overflow assume_none --enable witness.yaml.enabled
// Regression test for https://github.com/goblint/analyzer/issues/2159
int main(void) {
  int x, z;
  x = z + 1;
  // should not hold: (long long x) + 1LL == (long long )z
  return 0;
}
