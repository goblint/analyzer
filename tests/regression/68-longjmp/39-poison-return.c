// PARAM: 
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <goblint.h>

jmp_buf env_buffer;

int wrap () {
   int val;
   int x;
   int top;

   val = val + 2; //NOWARN

   if(setjmp( env_buffer )) {
      x = val; //WARN
      return 3;
   };

   val = 8;
   longjmp(env_buffer, 2);

   __goblint_check(0); // NOWARN
   return(0);
}

int main() {
   wrap();
   int y = 8;
   wrap();
}
