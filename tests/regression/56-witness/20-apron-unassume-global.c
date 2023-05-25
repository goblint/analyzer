// SKIP PARAM: --set ana.activated[+] apron --set ana.activated[+] unassume --set witness.yaml.unassume 20-apron-unassume-global.yml
#include <assert.h>

int i = 0;
int main() {
  while (i < 100) {
    i++;
  }
  assert(i == 100);
  return 0;
}
