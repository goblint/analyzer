// SKIP PARAM: --set ana.activated[+] apron --set ana.activated[+] unassume --set witness.yaml.unassume 25-apron-unassume-strengthening.yml --enable ana.apron.strengthening --set sem.int.signed_overflow assume_none --set ana.apron.domain octagon
#include <goblint.h>

int main() {
  int x, y;
  x = 0;
  if (x <= y) {
    __goblint_check(x == 0); // UNKNOWN (intentional by unassume)
    __goblint_check(x >= 0);
    __goblint_check(x <= y);
  }

  // With type bounds we have y <= 2147483647.
  if (x < y) { // Assuming this implies x <= 2147483646 in closure.
    // Unassuming 0 <= x actually unassumes 0 <= x <= 2147483647.
    // Thus, strengthening cannot add x < y back.
    __goblint_check(x == 0); // UNKNOWN (intentional by unassume)
    __goblint_check(x >= 0);
    __goblint_check(x < y); // TODO? Used to work without type bounds: https://github.com/goblint/analyzer/issues/1373.
  }
  return 0;
}
