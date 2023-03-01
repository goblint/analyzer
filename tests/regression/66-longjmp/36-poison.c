// PARAM: --enable ana.int.interval
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

jmp_buf env_buffer;
int global = 0;

int fun() {
   longjmp(env_buffer, 2);
}


int main () {
   int val;
   int x;

   __goblint_check(global == 0);
   if(setjmp( env_buffer )) {
      x = val; // WARN
      return;
   };

   val = 8;
   fun();

   __goblint_check(0); // NOWARN
   return(0);
}
