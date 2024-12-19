//SKIP PARAM:  --enable ana.int.interval  --set sem.int.signed_overflow assume_none  --set ana.activated[+] lin2vareq
// motivated from SVCOMP's terminator_02-1.c
// checks, whether the lin2var interval refinement meddles with the wrong bounds

#include <goblint.h>

int main()
{
  int x;
  if(x<100) 
  {
    __goblint_check(x<100); //SUCCESS
    x=x;
  }                           
  __goblint_check(x>=-2147483647); //UNKNOWN
  return 0;
}


