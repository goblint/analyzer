// PARAM: --set ana.activated[+] unassume --set witness.yaml.unassume 42-base-unassume-precheck.yml --enable ana.int.interval
#include <goblint.h>
// ana.unassume.precheck not required? propagation unassume automatically skips?
int main() {
  int i;
  i = 0;
  __goblint_check(i == 0);
  return 0;
}