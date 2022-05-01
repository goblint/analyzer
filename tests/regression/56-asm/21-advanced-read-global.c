#include <assert.h>

int x = 0;
int y = 0;

int main()
{
  asm("nop"
      :
      : "r"(x));

  assert(x == 0);
  assert(y == 0);

  return 0;
}
