// SKIP PARAM: --set ana.activated[+] apron --set ana.activated[+] unassume --set witness.yaml.unassume 12-apron-unassume-branch.yml
#include <assert.h>
// TODO: this was supposed to test local widening tokens, but thanks to apron narrowing works without
int main() {
  int i = 0;
  while (i < 100) {
    i++;
  }
  assert(i == 100);
  return 0;
}
