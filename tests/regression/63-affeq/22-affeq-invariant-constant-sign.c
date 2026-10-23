// SKIP PARAM: --set ana.activated[+] affeq --set sem.int.signed_overflow assume_none --enable witness.yaml.enabled
// CRAM test for https://github.com/goblint/analyzer/issues/2159

int main(void) {
  int x, z;
  x = z + 1;
  return 0;
}
