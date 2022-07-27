// SKIP PARAM: --set ana.activated[+] apron --set ana.activated[+] unassume --set witness.yaml.unassume 12-apron-unassume-branch.yml
#include <assert.h>

int main() {
  int i = 0;
  while (i < 100) {
    i++;
  }
  assert(i == 100); // TODO: avoid widening when unassume inside loop
  return 0;
}
