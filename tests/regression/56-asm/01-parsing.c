#include <assert.h>

int main()
{
  int x;

  asm("nop");

  asm volatile("");

  assert(1);

  return 0;
}
