#include <stdlib.h>

int main(void) {
  char *x = malloc(64);
  char *y = x;
  asm ("nop" : "=x" (*x));
  free(y);
  return 0;
}
