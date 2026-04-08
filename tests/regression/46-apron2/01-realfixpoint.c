// SKIP PARAM: --set ana.base.arrays.domain partitioned --enable ana.int.interval  --set ana.activated[+] apron
#include <goblint.h>
int main ()
{
  char A [3];

  A[2] = 0;

  char *str = A;
  int i = 0;

  while (str[i] != 0) {
    __goblint_check(1); // reachable    
    i++;
  } 

  return 0;
}
