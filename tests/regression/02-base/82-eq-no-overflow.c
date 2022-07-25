// PARAM: --enable ana.int.interval
#include <assert.h>

int main() {
  unsigned long x;
  x = 0;

  int b;
  b = x == 7; // NOWARN

  if (b)
    assert(0); // NOWARN (unreachable)
  else
    assert(1); // reachable

  return 0;
}