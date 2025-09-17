// PARAM: --set ana.activated[+] unassume --set witness.yaml.unassume 44-base-unassume-array.yml --enable ana.int.interval
#include <goblint.h>

int main() {
  int a[10];

  for (int i = 0; i < 3; i++) {
    a[i] = i;
  }

  for (int i = 0; i < 10; i++) {
    __goblint_check(a[i] >= 0);
    __goblint_check(a[i] < 3);
  }
  return 0;
}
