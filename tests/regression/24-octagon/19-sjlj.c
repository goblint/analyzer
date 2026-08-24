//SKIP PARAM: --enable ana.int.interval --set ana.activated[+] apron
// See https://github.com/goblint/analyzer/issues/1774
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <goblint.h>

jmp_buf env_buffer;

int fun() {
   longjmp(env_buffer, 2);
}


int main () {
   int val;

   val = setjmp(env_buffer);
   if(val) {
      __goblint_check(1); // Reachable
   } else {
      fun();
   }

   __goblint_check(1); // Reachable
   return(0);
}
