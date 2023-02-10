// PARAM: --enable ana.int.interval
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

jmp_buf env_buffer;
int global = 0;

int fun() {
   int top;

   if(top == 1) {
      longjmp(env_buffer, 2); //NOWARN
   } else if (top == 2) {
      longjmp(env_buffer, 0); //WARN
   } else {
      longjmp(env_buffer, top); //WARN
   }
}


int main () {
   int val;

   __goblint_check(global == 0);
   if(setjmp( env_buffer )) {
      return 8;
   }

   fun();

   __goblint_check(0); // NOWARN
   return(0);
}
