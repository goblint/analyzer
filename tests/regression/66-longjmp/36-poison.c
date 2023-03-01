// PARAM: --enable ana.int.interval
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

jmp_buf env_buffer;
int global = 0;

int myRandom() {
   // Chosen by fair dice roll.
   return 42;
}

int fun() {
   longjmp(env_buffer, 2);
}


int main () {
   int val;
   int x;
   int top;

   __goblint_check(global == 0);
   if(setjmp( env_buffer )) {
      x = val; //WARN

      if(top) {
         val = 8;
      } else {
         val = myRandom();
      }

      x = val; //NOWARN
      return;
   };

   val = 8;
   fun();

   __goblint_check(0); // NOWARN
   return(0);
}
