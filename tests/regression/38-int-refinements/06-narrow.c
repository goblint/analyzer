// PARAM: --set ana.int.refinement fixpoint --enable ana.int.interval
// FIXPOINT
#include<assert.h>

int g = 0;

void main()
{
   int i = 0;
   while (1) {
      i++;
      for (int j=0; j < 10; j++) {
         if (i > 100) g = 1;
      }
      if (i>9) i=0;
   }
   return;
}
