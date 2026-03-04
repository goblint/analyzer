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

  int c = rand();
  if (c) {
    h = &g;
  }

  int z = h(x, y);

  return z;
}

// no warnings here, since we cannot determine which function is called and y is used if h evaluates to f, so we have to assume that y is used