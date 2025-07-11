#include <goblint.h>

int main(void) {
    int x = 0;


   /*
   Only run this test with either of the while loops commented in.
   If both are commented in, the first check will proof the second.
   */

   while(x < 42 || x > 42) {
       x++;
       __goblint_check(x >= 1); // SUCC
   }


   // while(x != 42) {
   //     x++;
   //     __goblint_check(x >= 1); // UNKNOWN
   // }

}