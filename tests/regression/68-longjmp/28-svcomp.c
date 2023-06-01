// PARAM: --enable ana.int.interval --enable exp.earlyglobs --enable ana.sv-comp.enabled --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )"
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <goblint.h>

int main(void)
{
   jmp_buf buf;
   int top;

   if(top) {
      if(setjmp(buf)) {
         __goblint_check(1);
         return 8;
      }
   }

   longjmp(buf, 1); //WARN

   return 0;
}
