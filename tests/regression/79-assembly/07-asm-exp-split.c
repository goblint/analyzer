// PARAM: --set ana.activated[+] expsplit --disable asm_is_nop
#include <goblint.h>

int main(void) {
  int r, x;
  __goblint_split_begin(x);
  x = r ? 1 : 0;
  asm("nop" : "=x" (x), "=x" (r));
  __goblint_check(x == 0 || x == 1);
  __goblint_split_end(x);
  __goblint_check(x == 0 || x == 1); // UNKNOWN (intentionally)
}
