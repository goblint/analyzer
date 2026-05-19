// SKIP PARAM: --set ana.activated[+] var_eq --enable ana.int.interval --set ana.activated[+] unassume --set witness.yaml.unassume 74-var_eq-unassume-interval.yml
#include <assert.h>

int main() {
  int i = 0;
  while (i < 100) { // TODO: location invariant before loop doesn't work anymore
    i++;
  }
  assert(i == 100);
  return 0;
}
