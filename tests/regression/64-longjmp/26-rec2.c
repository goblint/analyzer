// SKIP PARAM: --enable ana.int.interval --enable exp.earlyglobs
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

jmp_buf env_buffer;
int global = 0;


void handler(int v) {
   if(setjmp( env_buffer )) {
      return 8;
   }


   if(v == 0) {
      handler(1);
   } else {
      fun();
   }
   
   fun();
}


int fun() {
   int top;
   meOrAlsoNotMe();
   longjmp(env_buffer, 2); //WARN
}

void bar() {

   if(global == 0) {
      if(setjmp( env_buffer )) {
         return 8;
      }
   }

   if(global == 0) {
      if(setjmp( env_buffer )) {
         return 8;
      }
   }

   global++;
   bar();
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
