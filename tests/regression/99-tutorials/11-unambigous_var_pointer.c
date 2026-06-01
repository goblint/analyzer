// SKIP TERM PARAM: --enable ana.wp_run 
#include <stdlib.h>

int main() {
  int x = 1;
  int *p = &x;

  x = 2;
  *p = 3; // x is now 3 and should not be live before this point anymore, but p is still live  
  int* q = p; //this assignment is unnecesary and p is not live after this point
  int z = 2 * (*p); // x and p are live before this point 

  return z;
}

/* This only works if we assume that if the query MayPointTo returns a single variable, then this variable is definietely the one pointed to.
 */

 // There is a problem since x should not be live just because its address is taken