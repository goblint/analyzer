// SKIP PARAM: --set ana.activated[+] apron --set ana.activated[+] unassume --set witness.yaml.unassume 43-apron-unassume-precheck.yml --enable ana.unassume.precheck
#include <goblint.h>

int main() {
  int i;
  i = 0;
  __goblint_check(1); // padding
  __goblint_check(i < 9);
  return 0;
}