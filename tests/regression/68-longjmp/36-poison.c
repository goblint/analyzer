// PARAM: --enable ana.int.interval
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <goblint.h>

jmp_buf env_buffer;
int global = 0;

int myRandom() {
   // Chosen by fair dice roll.
   return 42;
}

int funjmp() {
   longjmp(env_buffer, 2);
}

int fun(int param) {
   param = param +1; //NOWARN
   if(param == 2) {
      funjmp();
   }
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
      fun(5);
      return;
   };

   val = 8;
   fun(1);

   __goblint_check(0); // NOWARN
   return(0);
}
