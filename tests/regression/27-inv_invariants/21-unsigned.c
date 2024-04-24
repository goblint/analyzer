//PARAM:  --enable ana.int.interval
#include <stdio.h>
#include <stdlib.h>
#include <time.h>


int main() {
   int i = 0;
   unsigned int length = 5;

   while(i < length) {
      i = i+1;
   }

   __goblint_check(i == 5);

   return 0;
}
