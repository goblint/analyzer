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
      : "=r"(x), "=r"(y), "=r"(gx), "=r"(gy));

  assert(x == 0); // UNKNOWN
  assert(y == 0); // UNKNOWN
  assert(z == 0);
  assert(gx == 0); // UNKNOWN
  assert(gy == 0); // UNKNOWN
  assert(gz == 0);

  return 0;
}
