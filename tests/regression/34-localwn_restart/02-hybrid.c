// PARAM: --enable ana.int.interval --set solver td3 --enable solvers.td3.restart.wpoint.enabled --disable solvers.td3.restart.wpoint.once --set sem.int.signed_overflow assume_none
// ALSO:  --enable ana.int.interval --set solver sl4 --set sem.int.signed_overflow assume_none
// Example from Amato-Scozzari, SAS 2013, based on Halbwachs-Henry, SAS 2012.
// Localized narrowing with restart policy should be able to prove that
// 0 <= i <= 10 inside the inner loop.
#include <assert.h>

void main()
{
   int i = 0;
   while (1) {
      i++;
      for (int j=0; j < 10; j++) {
         __goblint_check(0 <= i);
         __goblint_check(i <= 10);
      }
      if (i>9) i=0;
   }
   return;
}
