// SKIP PARAM: --set ana.activated[+] apron --set ana.apron.domain polyhedra --set ana.activated[+] unassume --set witness.yaml.unassume 10-apron-unassume-interval.yml
#include <assert.h>
// Using polyhedra instead of octagon, because the former has no narrowing and really needs the witness.
int main() {
  int i = 0;
  while (i < 100) {
    i++;
  }
  assert(i == 100);
  return 0;
}
