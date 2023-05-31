// SKIP PARAM: --enable ana.int.interval_set --set solver td3 --set ana.activated[+] apron --set sem.int.signed_overflow assume_none
// This is part of 34-localization, but also symlinked to 36-apron.

// ALSO:  --enable ana.int.interval_set --set solver slr3 --set ana.activated[+] apron --set sem.int.signed_overflow assume_none
// Example from Halbwachs-Henry, SAS 2012
// Localized widening or restart policy should be able to prove that i <= j+3
// if the abstract domain is powerful enough.
#include <goblint.h>>

void main()
{
   int i = 0;
   while (i<4) {
      int j=0;
      while (j<4) {
         i=i+1;
         j=j+1;
      }
      i = i-j+1;
      __goblint_check(i <= j+3);
   }
   return ;
}
