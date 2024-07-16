//PARAM: --enable ana.int.interval --enable exp.earlyglobs
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <goblint.h>

jmp_buf env_buffer;
int global = 0;


void handler(int v) {
   if(setjmp( env_buffer )) {
      return 8;
   }


   if(v == 0) {
      handler(1);
   } else {
      fun();
   }

   // If v == 0 then env_buffer was set in the recursive call to handler
   // meaning that jumping here again is not valid.
   fun();
}


int fun() {
   int top;
   longjmp(env_buffer, 2); //WARN
}

int main () {
   int val;

   handler(global);
   return(0);
}
