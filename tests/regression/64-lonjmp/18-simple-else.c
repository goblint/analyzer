#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

jmp_buf env_buffer;
int global = 0;

int fun() {
   global = 2;
   longjmp(env_buffer, 2);
}


int main () {
   int val;
   jmp_buf env_buffer2;

   /* save calling environment for longjmp */
   val = setjmp( env_buffer );

   if( val != 0 ) {
      printf("Returned from a longjmp() with value = %i\n", val);
      __goblint_check(global == 2);
      exit(0);
   }

   fun();
   __goblint_check(0);

   return(0);
}
