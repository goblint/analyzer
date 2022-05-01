#include <assert.h>

int gx = 0;
int gy = 0;

int main()
{
  int x = 0;
  int y = 0;

  asm("nop"
      :
      : "r"(x), "r"(gx));

  assert(x == 0);
  assert(y == 0);
  assert(gx == 0);
  assert(gy == 0);

  return 0;
}
