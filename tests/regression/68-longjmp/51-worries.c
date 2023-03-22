// PARAM: --enable ana.int.interval
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <goblint.h>

jmp_buf env_buffer;
int global = 0;

int bar() {
   longjmp(env_buffer, 2);
   return 8;
}

void foo() {
   global = bar();
}

int main() {
   if(setjmp( env_buffer )) {
      assert(global == 0);
      return 0;
   }

   foo();
}
