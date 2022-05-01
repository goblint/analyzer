#include <assert.h>

int main()
{
  int x = 0;
  int y;

  if (!y)
  {
    asm("movl $1, %0"
        : "=r"(x));
  }

  assert(x == 1); // UNKNOWN

  return 0;
}
