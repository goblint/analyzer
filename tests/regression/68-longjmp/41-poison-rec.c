// PARAM: --enable ana.int.interval
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <goblint.h>

jmp_buf env_buffer;
int global = 0;

int fun(int param) {
   char keyword[81] = "example";

   if(param != 2) {
      char c = keyword[0]; //NOWARN
      return 1;
   }

   if(setjmp( env_buffer )) {
      fun(4);

      char c = keyword[0]; //WARN
      return 3;
   };

   keyword[0] = 'a';

   longjmp(env_buffer, 2);
}

int main () {
   fun(2);
   return(0);
}
