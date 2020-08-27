// PARAM: --html --enable dbg.showtemps --enable ana.int.interval -v --enable dbg.debug --enable dbg.verbose --enable exp.partition-arrays.enabled
#include <assert.h>

int g[2];

int main() {
  int x, y;
  y = x; // WARN uninitialized (only one detected by -Wuninitialized)
  // y = bot but both branches should be reachable
  if (y) assert(1);
  else assert(1);

  int a[1];
  y = a[0]; // WARN uninitialized
  a[0] = 0;
  y = a[0]; // NOWARN (singleton)
  assert(y == 0);

  int b[2];
  y = b[0]; // WARN uninitialized
  b[0] = 0; // here we use exp.partition-arrays.enabled, without it and arrays with 2 or more elements we do not know which indices have been initialized!
  y = b[0]; // NOWARN
  y = b[1]; // WARN uninitialized
  if (y) assert(1);
  else assert(1);

  // globals are initialized with 0, so this is ok:
  y = g[0]; // NOWARN
  return x;
}
