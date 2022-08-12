// PARAM: --enable ana.int.interval --set solver td3
// ALSO: --enable ana.int.interval --set solver slr3
// Example from Halbwachs-Henry, SAS 2012
// Localized widening should be able to prove that i=10 at the end
// of the nested loops.
#include <assert.h>

void main()
{
   int i = 0;

   for (; i<10 ; i++) {
     for (int j = 0; j < 10 ; j++) ;
   }

   __goblint_check(i == 10);
}
