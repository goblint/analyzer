#include <assert.h>

int main()
{
  struct test
  {
    int a;
    int b;
  } x = {0, 0};
  int y;

  if (!y)
  {
    asm("movl $1, %0"
        : "=r"(x.a));
  }

  assert(x.a == 1); // UNKNOWN
  assert(x.b == 0);

  return 0;
}
