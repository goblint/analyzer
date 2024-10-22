// SKIP PARAM: --set ana.activated[+] apron --set ana.activated[+] unassume --set witness.yaml.unassume 48-apron-unassume-unsound-int-poly.yml --enable ana.apron.strengthening --set sem.int.signed_overflow assume_none --set ana.apron.domain polyhedra
#include <goblint.h>

int main() {
  int x, y;
  if (x == 2 * y) {
    __goblint_check(x == 2 * y); // UNKNOWN (intentional by unassume)
    __goblint_check(x == 2 * y); // UNKNOWN (intentional by unassume)
  }
  return 0;
}
