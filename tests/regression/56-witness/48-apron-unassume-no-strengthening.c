// SKIP PARAM: --set ana.activated[+] apron --set ana.apron.domain interval --set ana.activated[+] unassume --set witness.yaml.unassume 48-apron-unassume-no-strengthening.yml --disable ana.apron.strengthening
#include <goblint.h>

int main() {
  int i = 0;
  int j;
  if (j < 100) {
    __goblint_check(i == 0); // UNKNOWN (intentional by unassume)
    __goblint_check(i >= 0);
    __goblint_check(i < 100);
    __goblint_check(j < 100);
  }
  return 0;
}
