// PARAM: --set ana.activated[+] affeq --set ana.activated[+] apron --set ana.autotune.enabled true --set ana.autotune.activated '["loopUnrollHeuristic"]' --set sem.int.signed_overflow assume_none --enable witness.yaml.enabled
// Regression test for https://github.com/goblint/analyzer/issues/2159:
// the affine-equality Apron invariant exporter must not flip the sign of the
// constant term, otherwise the emitted witness invariant is contradictory.
long long x = 1;
int main(void) {
  int z;
  while (1)
    x = z + 1;
  return 0;
}
