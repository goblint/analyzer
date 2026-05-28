// SKIP TERM PARAM: --enable ana.wp_run 
#include <stdlib.h>

int f(int a, int b) {
  return a + 1;
}

int g(int a, int b) {
  return b + 1;
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

// no warnings here, since we cannot determine which function is called