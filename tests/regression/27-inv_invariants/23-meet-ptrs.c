//PARAM:  --enable ana.int.interval
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int more_intricate() {
   int arr[20];

   int top;

   int i = 2;
   int j = 8;
   if(top) {
      i = 8;
      j = 9;
   }

   int* imprecise1 = &arr[i]; // &arr[2..8]
   int* imprecise2 = &arr[j]; // &arr[8..9]

   if(imprecise1 == imprecise2) {
      __goblint_check(imprecise1 == &arr[8]);
      __goblint_check(imprecise2 == &arr[8]); //TODO (Refinement should happen in both directions!)
   }

   if(imprecise1 == &arr[j]) {
      __goblint_check(imprecise1 == &arr[8]);
   }

}


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

   more_intricate();
   return 0;
}
