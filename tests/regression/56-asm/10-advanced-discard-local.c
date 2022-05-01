#include <assert.h>

int main()
{
  int x = 0;
  int y = 0;

  asm("nop"
      : "=r"(x));

  assert(x == 0); // UNKNOWN
  assert(y == 0);

  return 0;
}
