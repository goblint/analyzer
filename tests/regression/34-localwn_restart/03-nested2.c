// PARAM: --enable ana.int.interval --set solver td3 --set sem.int.signed_overflow assume_none
// ALSO:  --enable ana.int.interval --set solver slr3 --set sem.int.signed_overflow assume_none
// Example from Amato-Scozzari, SAS 2013
// Localized narrowing should be able to prove that i >= 0 in the outer loop.
#include <goblint.h>

void main()
{
   int i = 0;
   while (1) {
      int j = 0;
      for (; j<10; j++) ;
      i=i+11-j;
      __goblint_check(i >= 0);
   }
   return;
}
