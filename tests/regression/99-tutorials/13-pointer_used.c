// SKIP TERM PARAM: --enable ana.wp_run 
#include <stdlib.h>


int main()
{
  int x = 1;
  int y = 2;

  int *p = &x;

  *p = 3; // x is now 3 and should not be live before this point anymore
  int z = 2 * (*p);

  return z;
}

/* This only works if we assume that if the query MayPointTo returns a single variable, then this variable is definietely the one pointed to.
 */