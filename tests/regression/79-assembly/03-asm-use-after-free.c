//PARAM: --set ana.activated[+] useAfterFree --disable asm_is_nop
#include <stdlib.h>

int main(void) {
  int *x = malloc(16);
  free(x);
  // write
  asm ("nop" : "=x" (*x)); // WARN
  // read
  asm ("nop" : : "x" (*x)); // WARN
}
