// SKIP
// Out of bound accesses
#include <assert.h>
int main()
{
  int x, y, z[2];
  int *p = &x;
  ++ p;
  __goblint_check(p == &y); // UNKNOWN

  p = &z[-1];
  __goblint_check(p == &y); // UNKNOWN

  p = &z[y];
  __goblint_check(p == &z[y]); // UNKNOWN

  return 0;
}
