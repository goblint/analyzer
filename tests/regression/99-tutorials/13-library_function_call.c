// SKIP TERM PARAM: --enable ana.wp_run 
#include <stdio.h>

int main()
{
  int x = 1;

  printf("%d\n", x);
  return 1;
}

/* This should not report a warning, as x is used in a library function call.
 */
