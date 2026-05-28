// SKIP TERM PARAM: --enable ana.wp_run 
#include <stdlib.h>


int main()
{
  int x = 1; // this assignment is unnecessary, but not detected, since x is used in a right hand side of an assignment to a live variable

  int *p = &x
  ;

  *p = 3; // x is now 3 and should not be live before this point anymore, but p is still live  
  int z = 2 * (*p); // x and p are live before this point 

  int* w = p; //this assignment is unnecesary and p is not live after this point

  return z;
}

/* This only works if we assume that if the query MayPointTo returns a single variable, then this variable is definietely the one pointed to.
 */