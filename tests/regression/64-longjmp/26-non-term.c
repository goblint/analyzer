// SKIP PARAM: --enable ana.int.interval --enable exp.earlyglobs
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

jmp_buf env_buffer;
int global = 0;

// This will cause the analysis to not terminate as the set of
// active setjumps will grow without bounds as the set of active
// setjumps also forms part of the context.
void bar() {
   if(global == 0) {
      if(setjmp( env_buffer )) {
         return 8;
      }
   }

   bar();
}


int main () {
   bar();
   return(0);
}
