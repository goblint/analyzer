#include<stdlib.h>
#include<goblint.h>
int g;
int* global = &g;

void fun() {
  int top;
  int top2;
  int x;

  if(top) {
    global = &x;
  }

  x = 8;
  if(top2) {
    fun();
  }
  __goblint_check(x == 8); // UNKNOWN! We claim this holds, but it most not really hold: If top is false in the callee, it updates x to `5`

  // May dereference x of previous invocation, should probably warn
  *global = 5;
}

int main() {
  fun();

  *global = 8; //We should probably warn here for invalid deref (?)

  fun();
}