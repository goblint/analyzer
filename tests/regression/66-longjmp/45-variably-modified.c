// PARAM: --enable ana.int.interval --enable ana.int.enums --set ana.activated[+] expsplit
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <goblint.h>

jmp_buf env_buffer;
int global = 0;

int fun() {
   if(setjmp(env_buffer)) { //NOWARN
      return 0;
   }


   global = 2;
   longjmp(env_buffer, 2);
}


int main () {
   int val;
   jmp_buf env_buffer2;

   __goblint_check(global == 0);

   if(setjmp(env_buffer)) { //NOWARN
      return 0;
   }

   int n;

   {
      // Array of variably modified type
      int a[n];

      if(setjmp(env_buffer)) { // WARN
         return 0;
      }
   }

   {
      // Array of variably modified type
      int b[2][n];

      if(setjmp(env_buffer)) { // WARN
         return 0;
      }
   }

   fun();

   return(0);
}
