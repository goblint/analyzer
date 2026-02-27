// SKIP TERM PARAM: --enable ana.wp_run 
#include <stdlib.h>

int f(int a, int b) {
  
  if (a > 0) {
    return a + b;
  } else {
    return a;
  }

}

int g(int a, int b) {
  
  if (a < 0) {
    return a - b;
  } else {
    return a;
  }

}

int main()
{
  int x = 1;
  int y = 2;

  int (*h) (int, int) = &f;

  int c;
  if (c) {
    h = &g;
  }

  int z = h(x, y);

  return z;
}
