// PARAM: --set ana.activated[+] var_eq
#include <goblint.h>

int main() {
  int i, j;
  if (i == j)
    __goblint_check(i == j);
  return 0;
}
