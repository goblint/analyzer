// SKIP PARAM: --set ana.activated[+] unassume --set witness.yaml.unassume 16-base-unassume-dependent.yml --enable ana.int.interval
#include <assert.h>

int main() {
  int i, j;
  i = 0;
  j = 42;
  __goblint_check(i == 0); // TODO UNKNOWN (intentional by unassume)
  __goblint_check(j == 42); // TODO UNKNOWN (intentional by unassume)
  __goblint_check(0 <= i);
  __goblint_check(i <= 42);
  __goblint_check(0 <= j);
  __goblint_check(j <= 42);
  return 0;
}