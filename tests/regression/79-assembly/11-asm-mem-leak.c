//PARAM: --set ana.malloc.unique_address_count 1 --set ana.activated[+] memLeak --disable asm_is_nop
#include <stdlib.h>

void ok(void) {
  char *x = malloc(64);
  char *y = x;
  asm ("nop" : "=x" (x));
  free(y);
  return; //NOWARN
}

void not_ok(void) {
  char *x = malloc(64);
  asm ("nop" : "=x" (x));
  free(x); //WARN
  return;
}

int main(void) {
  ok();
  not_ok();
  return 0;
}
