// PARAM: --set ana.activated[+] unassume --set witness.yaml.unassume 49-base-unassume-narrow.yml --enable ana.int.interval --set ana.base.invariant.unassume fixpoint --set sem.int.signed_overflow assume_none --disable ana.int.def_exc
// TODO: why need assume_none? // disable def_exc to avoid implicit widening threshold 0
#include <goblint.h>
int main() {
  int i;
  i = 50;
  __goblint_check(i == 50); // UNKNOWN (intentional by unassume)
  __goblint_check(i <= 51); // UNKNOWN (intentional by unassume)
  __goblint_check(49 <= i); // UNKNOWN (intentional by unassume)
  __goblint_check(i <= 99);
  __goblint_check(0 <= i);
  return 0;
}
