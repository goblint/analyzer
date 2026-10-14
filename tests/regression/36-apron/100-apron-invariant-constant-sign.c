// PARAM: --set ana.activated[+] apron --set ana.apron.domain affeq --enable ana.int.interval --enable witness.yaml.enabled --set witness.yaml.invariant-types '["loop_invariant"]' --disable ana.base.invariant.enabled --enable ana.relation.invariant.one-var --disable ana.apron.invariant.diff-box --set sem.int.signed_overflow assume_none
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
