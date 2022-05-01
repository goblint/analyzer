#include <assert.h>

int x = 0;

int main()
{
  asm("nop");

  assert(x == 0); // UNKNOWN

  return 0;
}
