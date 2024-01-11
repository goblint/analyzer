#include <math.h>

int main(void) {
  int x = floor(0.8);
  asm ("nop" : "=x" (x));
  return x;
}
