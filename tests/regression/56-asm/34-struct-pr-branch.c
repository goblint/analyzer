#include <assert.h>

int main()
{
  struct test
  {
    int a;
    int b;
  };

  struct test v = {0, 0};
  struct test *x = &v;
  int y;

  if (!y)
  {
    asm("nop"
        :
        : "r"(x));
  }

  assert(x->a == 0); // UNKNOWN
  assert(x->b == 0); // UNKNOWN

  assert(v.a == 0); // UNKNOWN
  assert(v.b == 0); // UNKNOWN

  return 0;
}
