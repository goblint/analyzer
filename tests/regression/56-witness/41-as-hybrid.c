// PARAM: --enable ana.int.interval --set ana.activated[+] unassume --set witness.yaml.unassume 41-as-hybrid.yml
#include <goblint.h>
int main() {
  int i = 0;
  while (1) {
    i++;
    int j = 0;
    while (j < 10) {
      __goblint_check(0 <= i);
      __goblint_check(i <= 10);
      j++;
    }
    if (i > 9)
      i = 0;
  }
  return 0;
}
