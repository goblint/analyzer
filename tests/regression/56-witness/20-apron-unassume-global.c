// SKIP PARAM: --set ana.activated[+] apron --set ana.activated[+] unassume --set witness.yaml.unassume 20-apron-unassume-global.yml
#include <assert.h>

int i = 0;
int main() {
  while (i < 100) { // TODO: location invariant before loop doesn't work anymore
    i++;
  }
  assert(i == 100);
  return 0;
}
