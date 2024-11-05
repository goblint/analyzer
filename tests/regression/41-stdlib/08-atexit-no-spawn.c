// PARAM: --enable sem.atexit.ignore
#include <stdlib.h>
#include <goblint.h>

void bye()
{
  __goblint_check(0); // NOWARN (unreachable)
}

int main()
{
  atexit(bye);
  return 0;
}
