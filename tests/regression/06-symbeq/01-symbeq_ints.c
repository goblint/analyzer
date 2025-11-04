// PARAM: --enable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'"
#include<stdio.h>
#include <goblint.h>

int main() {
  int x;
  int y;

  scanf("%d", &x);
  y = x;

  __goblint_check(x==y);

  return 0;
}
