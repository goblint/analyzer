// PARAM: --enable ana.race.direct-arithmetic --set ana.activated[+] "'var_eq'" --disable asm_is_nop
#include<goblint.h>
#include<stdio.h>

void not_ok(int x) {
  int y = x;
  asm ("nop" : "=x" (x));
  __goblint_check(x == y); // UNKNOWN
}

void ok(int x) {
  asm ("nop" : "=x" (x));
  int y = x;
  __goblint_check(x == y);
}

int main() {
  int x;
  scanf("%d", &x);
  ok(x);
  not_ok(x);
}
