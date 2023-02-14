// SKIP PARAM: --enable ana.int.interval --enable exp.earlyglobs
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

jmp_buf env_buffer;
int global = 0;

int fun() {
   int top;
   longjmp(env_buffer, 2); //WARN
}

int bar() {
   int top,top2;

   if(top) {
      if(setjmp( env_buffer )) {
         return 8;
      }
   }

   if(top2) {
      fun();
   }
}


int main () {
   int val;

   __goblint_check(global == 0);
   bar();

   // In this second invocation of bar() the jumpbuffer could still contain the old value set during the first invocation, making the longjmp in fun()
   // an illegal longjmp :/
   bar(); //WARN

   __goblint_check(0); // NOWARN
   return(0);
}
