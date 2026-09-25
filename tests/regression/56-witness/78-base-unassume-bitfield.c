// PARAM: --enable ana.int.bitfield --set ana.activated[+] unassume --set witness.yaml.unassume 78-base-unassume-bitfield.yml
#include <goblint.h>

int main() {
  int a = 0;
  __goblint_check(a == 0); // TODO UNKNOWN (intentional by unassume)
  __goblint_check((a | 4) == 4);

  int b = 0;
  __goblint_check(b == 0); // TODO UNKNOWN (intentional by unassume)
  __goblint_check((b & 4) == 0);
  return 0;
}
