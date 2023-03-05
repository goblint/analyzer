// PARAM: --enable ana.int.interval
#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>
#include <goblint.h>

jmp_buf env_buffer;
int global = 0;

int myRandom() {
   // Chosen by fair dice roll.
   return 42;
}

int funjmp() {
   longjmp(env_buffer, 2);
}

int fun(int param) {
   param = param +1; //NOWARN
   if(param == 2) {
      funjmp();
   }
}

int main () {
   int val;
   int x;
   int top;
   int* ptr = &val;
   int* ptr2 = &val;
   int** ptrptr = &ptr;
   int arr[10] = {0,0,0,0,0,0,0,0,0,0};
   int* ptr3;

   __goblint_check(global == 0);
   if(setjmp( env_buffer )) {
      x = val; //WARN
      x = *ptr; //WARN
      ptr3 = ptr2; //WARN
      x = *ptr2; //WARN
      x = **ptrptr; //WARN
      x = arr[**ptrptr]; //WARN

      *ptr = 5; //NOWARN
      x = *ptr; //NOWARN
      *ptr2 = 9; //WARN (ptr2 still has indeterminate value)

      x = val; //NOWARN
      fun(5);
      return 3;
   };

   val = 8;
   ptr2 = &x;
   fun(1);

   __goblint_check(0); // NOWARN
   return(0);
}
