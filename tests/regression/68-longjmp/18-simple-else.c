// PARAM: --enable ana.int.interval --enable ana.int.enums --set ana.setjmp.split none
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <goblint.h>

jmp_buf env_buffer;
int global = 0;

int fun() {
   global = 2;
   longjmp(env_buffer, 2);
}


int main () {
   int val;
   jmp_buf env_buffer2;

   __goblint_check(global == 0);

   /* save calling environment for longjmp */
   val = setjmp( env_buffer );

   __goblint_check(global == 0); //UNKNOWN!
   __goblint_check(global == 2); //UNKNOWN!
   __goblint_check(global == 8); //FAIL!

   if( val != 0 ) {
      printf("Returned from a longjmp() with value = %i\n", val);
      __goblint_check(val == 2);
      __goblint_check(global == 2); //TODO (requires path-sensitivity distinguishing between returns) -> see test 44
      exit(0);
   }

   fun();

   __goblint_check(0); // NOWARN
   return(0);
}
