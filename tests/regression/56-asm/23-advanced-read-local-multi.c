#include <assert.h>

int main()
{
  int x = 0;
  int y = 0;
  int z = 0;

  asm("nop"
      :
      : "r"(x), "r"(y));

  assert(x == 0);
  assert(y == 0);
  assert(z == 0);

  return 0;
}
