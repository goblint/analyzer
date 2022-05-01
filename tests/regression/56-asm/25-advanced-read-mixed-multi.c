#include <assert.h>

int gx = 0;
int gy = 0;
int gz = 0;

int main()
{
  int x = 0;
  int y = 0;
  int z = 0;

  asm("nop"
      :
      : "r"(x), "r"(y), "r"(gx), "r"(gy));

  assert(x == 0);
  assert(y == 0);
  assert(z == 0);
  assert(gx == 0);
  assert(gy == 0);
  assert(gz == 0);

  return 0;
}
