#include <stdio.h>

int main(void) {
  FILE *f = fopen("file-use.c", "r");
  //int x;
  //asm ("nop" : "=g" (f), "=g" (x));
  fclose(f);
  return 0;
}
