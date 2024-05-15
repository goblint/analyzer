//PARAM:  --enable ana.int.interval
#include <stdio.h>
#include <stdlib.h>
#include <time.h>


int main() {
   int arr[20];

   int top;

   int i = 2;
   if(top) {
      i = 8;
   }

   int* imprecise = &arr[i];

   if(imprecise == &arr[2]) {
      __goblint_check(imprecise == &arr[2]);
   }

   return 0;
}
