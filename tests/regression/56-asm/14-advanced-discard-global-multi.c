#include <assert.h>

int x = 0;
int y = 0;
int z = 0;

int main()
{
  asm("nop"
      : "=r"(x), "=r"(y));

  assert(x == 0); // UNKNOWN
  assert(y == 0); // UNKNOWN
  assert(z == 0);

  return 0;
}
