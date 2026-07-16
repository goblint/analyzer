// PARAM: --set ana.activated[+] var_eq --set ana.activated[+] unassume --set witness.yaml.unassume 75-var_eq-unassume-equal.yml
#include <goblint.h>

int main() {
  int i, j;
  i = j;
  __goblint_check(i == j); // var_eq unassume should keep i == j by assuming it in naive unassume
  return 0;
}
