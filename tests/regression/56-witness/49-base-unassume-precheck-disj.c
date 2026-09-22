// PARAM: --set ana.activated[+] unassume --set witness.yaml.unassume 49-base-unassume-precheck-disj.yml --enable ana.int.interval --enable ana.unassume.precheck
#include <goblint.h>

int main() {
  int i;
  i = 0;
  __goblint_check(i == 0); // TODO
  return 0;
}