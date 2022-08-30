// PARAM: --set ana.activated[+] unassume --set witness.yaml.unassume 18-base-unassume-contra.yml --enable ana.int.interval
#include <assert.h>

int main() {
  int i;
  i = 0;
  __goblint_check(i == 0);
  return 0;
}