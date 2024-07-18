#include <stdlib.h>
#include <goblint.h>

void bye()
{
  __goblint_check(1); // reachable
}

int main()
{
  atexit(bye);
  return 0;
}
