//SKIP PARAM: --enable ana.int.interval --set ana.activated[+] var_eq
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
      // With var_eq not doing proper invalidation, the check below was unknown (due to the internal state being inconsistent), instead of failing as expected
      __goblint_check(val == 0); //FAIL
      __goblint_check(1); // Reachable
   } else {
      if(val == 0) {
         val = 0;
         fun();
      }
   }

   __goblint_check(1); // Reachable
   return(0);
}
