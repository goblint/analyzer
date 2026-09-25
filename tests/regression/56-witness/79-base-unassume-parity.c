// PARAM: --enable ana.int.def_exc --enable ana.int.congruence --set ana.activated[+] unassume --set witness.yaml.unassume 79-base-unassume-parity.yml
#include <goblint.h>

int main() {
  int a = 0;
  __goblint_check(a == 0); // TODO UNKNOWN (intentional by unassume)
  __goblint_check((a & 1) == 0);

  int b = 1;
  __goblint_check(b == 1); // TODO UNKNOWN (intentional by unassume)
  __goblint_check((b & 1) == 1);

  int a2 = 0;
  __goblint_check(a2 == 0); // UNKNOWN (intentional by unassume)
  __goblint_check(a2 % 2 != 1); // TODO

  int b2 = 1;
  __goblint_check(b2 == 1); // UNKNOWN (intentional by unassume)
  __goblint_check(b2 % 2 != 0); // TODO
  return 0;
}
