// PARAM: --enable ana.int.interval --set solver slr4
// Example from Amato-Scozzari, SAS 2013
// Localized narrowing with restart policy should be able to prove that
// 0 <= i <= 10 inside the inner loop.
#include <assert.h>

void main()
{
   int i = 0;
   while (1) {
      i++;
      for (int j=0; j < 10; j++) {
         __goblint_check(0 <= i); // UNKNOWN
         __goblint_check(i <= 10);
      }
      if (i>9) i=0;
   }
   return;
}
