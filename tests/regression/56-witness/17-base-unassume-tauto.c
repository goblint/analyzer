// PARAM: --set ana.activated[+] unassume --set witness.yaml.unassume 17-base-unassume-tauto.yml --enable ana.int.interval --set ana.base.invariant.unassume fixpoint
#include <assert.h>

int main() {
  int i;
  i = 0;
  __goblint_check(i == 0); // UNKNOWN (intentional by unassume)
  __goblint_check(i <= 1); // UNKNOWN (intentional by unassume)
  __goblint_check(-1 <= i); // UNKNOWN (intentional by unassume)
  return 0;
}