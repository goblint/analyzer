// PARAM: --set "ana.activated[+]" pentagon

// Taken from tests/regression/66-interval-set-one/73-intervals.c
#include <goblint.h>

int main(void) {
    int x = 0;

   while(x < 42 || x > 42) {
       x++;
       __goblint_check(x >= 1); // SUCC
   }


   while(x != 42) {
       x++;
       __goblint_check(x >= 1); // UNKNOWN
   }

}