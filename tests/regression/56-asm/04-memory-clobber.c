#include <assert.h>

int x = 0;

int main()
{
  asm("movl $1, x" : : : "memory");

  assert(x == 1); // UNKNOWN

  return 0;
}
