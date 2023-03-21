// PARAM: --enable ana.int.interval
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <goblint.h>

jmp_buf env_buffer;
int global = 0;

int fun() {
   longjmp(env_buffer, 2);
}


int main () {
   int val;

   __goblint_check(global == 0);
   setjmp( env_buffer );
   fun();

   __goblint_check(0); // NOWARN
   return(0);
}
