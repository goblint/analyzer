#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

void f(void) {
  assert(0); // FAIL!
  printf("hello, this is \"f\"\n");
}

void call_f(void)
{
  f();
  printf("hello, this is \"call_f\"\n");
  exit(0);
}

int main()
{
  call_f();
  printf("hello, this is \"main\"\n");
  return 0;
}
