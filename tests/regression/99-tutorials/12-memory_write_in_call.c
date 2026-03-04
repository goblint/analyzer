// SKIP TERM PARAM: --enable ana.wp_run 
#include <stdlib.h>

int f(int a, int* b) {
  b[0] = a ;
  return 0;
}


int main()
{
  int x = 1;
  int *p = malloc(sizeof(int));

  f(x, p);

  return x;
}
