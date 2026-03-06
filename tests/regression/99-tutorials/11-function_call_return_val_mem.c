// SKIP TERM PARAM: --enable ana.wp_run 
#include <stdlib.h>

int f(int a, int b) {
  
  if (a > 0) {
    return a + b;
  } else {
    return a;
  }

}

int main()
{
  int i = 1;
  int x = -1;
  int y = 2;
  int *p = malloc(sizeof(int) * i);

  p[0] = f(x, y);

  return x;
}
