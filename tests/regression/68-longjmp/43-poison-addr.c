// PARAM:
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <goblint.h>

jmp_buf env_buffer;

void makeFive(int* ptr) {
   *ptr = 5;
}

int wrap () {
   int val;
   int x;
   int top;

   val = val + 2; //NOWARN

   if(setjmp( env_buffer )) {
      makeFive(&val); //NOWARN
      x = val; //NOWARN
      return 3;
   };

   val = 8;
   longjmp(env_buffer, 2);

   __goblint_check(0); // NOWARN
   return(0);
}

int main() {
   wrap();
   int y = 8;
   wrap();
}
