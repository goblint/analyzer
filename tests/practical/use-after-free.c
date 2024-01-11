#include <stdlib.h>

int main(void) {
  int *x = malloc(16);
  free(x);
  asm ("nop" : "=x" (*x));
}
