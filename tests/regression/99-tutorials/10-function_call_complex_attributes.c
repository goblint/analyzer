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
  int x = 1;
  int y = 2;

  int z = f(x + (y * y), 0);

  return z;
}

// now warnings here. Both variables are relevant as they are used in an expression passed to a live parameter of the function call.