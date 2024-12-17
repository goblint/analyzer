//SKIP PARAM:  --enable ana.int.interval  --set sem.int.signed_overflow assume_none
// motivated from SVCOMP's terminator_02-1.c
// checks, whether the lin2var interval refinement meddles with the wrong bounds

#include <goblint.h>

int main()
{
  int x;
  if(x<100) 
  {
    x=x;
  }                           
  __goblint_check(x<100); //UNKNOWN
  __goblint_check(x>=-2147483647); //FAIL
  return 0;
}


