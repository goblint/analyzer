//PARAM: --set ana.setjmp.split coarse
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <goblint.h>

void jmpfunction(jmp_buf env_buf) {
   longjmp(env_buf, 2);
}

int main () {
   int val;
   jmp_buf env_buffer;

   /* save calling environment for longjmp */
   val = setjmp( env_buffer ); // NOWARN

   if( val != 0 ) { // NOWARN
      printf("Returned from a longjmp() with value = %i\n", val);
      longjmp(env_buffer, val+1);
      exit(0);
   }

   printf("Jump function call\n");
   jmpfunction( env_buffer );

   __goblint_check(0); // NOWARN

   return(0);
}
