#include <goblint.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * function f should be called
 * and the program should terminate
 */

void f(void) {
  __goblint_check(0); // FAIL!
  printf("hello, this is \"void f(void)\"\n");
}

void call_f(void)
{
  f();
  exit(0);
}

int main()
{
  while (1) {
    call_f();
  }

  return 0;
}
