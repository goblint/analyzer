// SKIP PARAM: --set ana.activated[+] apron --set ana.activated[+] unassume --set witness.yaml.unassume 12-apron-unassume-branch.yml --set ana.apron.domain polyhedra --enable ana.widen.tokens
#include <assert.h>
// Uses polyhedra instead of octagon such that widening tokens are actually needed by test instead of narrowing.
int main() {
  int i = 0;
  while (i < 100) {
    i++;
  }
  assert(i == 100);
  return 0;
}
