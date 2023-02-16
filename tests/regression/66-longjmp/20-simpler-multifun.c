// PARAM: --enable ana.int.interval
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

jmp_buf env_buffer;
int global = 0;

void foo() {
   int local = 7;
   longjmp(env_buffer, 2);
}


int fun() {
   foo();
}


int main () {
   int val;

   __goblint_check(global == 0);
   setjmp( env_buffer );
   fun();

   __goblint_check(0); // NOWARN
   return(0);
}
