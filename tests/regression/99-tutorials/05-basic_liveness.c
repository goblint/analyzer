// SKIP TERM PARAM: --enable ana.wp_run 
#include <stdlib.h>

int main()
{
  int x = 1;
  int y = 2;
  int z = 3;

  int a = rand();

  if (a) {
    x = x + y;
  } 

  return x;
}
