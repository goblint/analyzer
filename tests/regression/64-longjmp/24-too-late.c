// PARAM: --enable ana.int.interval
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

jmp_buf env_buffer;
int global = 0;

int fun() {
   int top;
   longjmp(env_buffer, 2); //WARN
}

int bar() {
   if(setjmp( env_buffer )) {
      return 8;
   }
}


int main () {
   int val;

   __goblint_check(global == 0);
   bar();
   fun();

   __goblint_check(0); // NOWARN
   return(0);
}
