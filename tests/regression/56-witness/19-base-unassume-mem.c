// PARAM: --set ana.activated[+] unassume --set witness.yaml.unassume 19-base-unassume-mem.yml --enable ana.int.interval
#include <assert.h>

int main() {
  int i, j;
  int *p;
  int r; // rand
  i = 0;
  j = 0;
  if (r)
    p = &i;
  else
    p = &j;
  __goblint_check(i == 0); // UNKNOWN (intentional by unassume)
  __goblint_check(j == 0); // UNKNOWN (intentional by unassume)
  __goblint_check(i >= 0);
  __goblint_check(j >= 0);
  __goblint_check(i <= 10);
  __goblint_check(j <= 10);
  return 0;
}