// PARAM: --enable ana.int.interval
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <goblint.h>

jmp_buf env_buffer;
int global = 0;

void foo() {
   int local = 7;
   longjmp(env_buffer, 2);
}


int fun() {
   global = 42;
   foo();
}


int main () {
   int val;

   __goblint_check(global == 0);
   if(0 == setjmp( env_buffer )) {
      fun();
   } else {
      __goblint_check(global == 42);
   }

   return(0);
}
