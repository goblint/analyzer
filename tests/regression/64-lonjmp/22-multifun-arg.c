// PARAM: --enable ana.int.interval --disable exp.volatiles_are_top
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

jmp_buf env_buffer;
int global = 0;

void foo() {
   int local = 7;
   longjmp(env_buffer, 2);
}


int fun(int* ptr) {
   global = 42;
   *ptr = 1;
   foo();
}


int main () {
   volatile int val = 0;

   __goblint_check(global == 0);
   if(0 == setjmp( env_buffer )) {
      fun(&val);
   } else {
      __goblint_check(val == 1);
      __goblint_check(global == 42); //TODO
   }

   return(0);
}
