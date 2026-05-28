// SKIP TERM PARAM: --enable ana.wp_run 
#include <stdlib.h>


int main()
{
  int x = 1;
  int y = 2;
  int* p;

  int c = rand() % 2; 
  // at this point, the set of live variables should be {x, y, c}
  if (c) {  
    p = &x;
  } else {
    p = &y;
  }
  // at this point, the set of live variables should be {x, y, p}

  int z = *p; 
  return z;
}

/* This should not report a warning, as both x and y may be live.
 */