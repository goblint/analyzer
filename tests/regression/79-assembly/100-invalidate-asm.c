//PARAM: --disable asm_is_nop
#include <goblint.h>

int main(void) {
  int x = 0;
  asm ("nop" : "=x" (x));
  __goblint_check(x == 0); //WARN
  return 0;
}
