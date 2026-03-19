// SKIP PARAM: --set ana.activated[+] apron --set ana.apron.domain polyhedra --set ana.activated[+] unassume --set witness.yaml.unassume 76-apron-unassume-extra-trivial.yml
#include <assert.h>
// Using polyhedra instead of octagon, because the former has no narrowing and really needs the witness.
int main() {
  int i = 0;
  while (i < 100) { // TODO: location invariant before loop doesn't work anymore
    i++;
  }
  assert(i == 100);
  return 0;
}
