// SKIP
// Out of bound accesses 
#include <assert.h>
int main()
{
  int x, y, z[2];
  int *p = &x;
  ++ p;
  assert(p == &y); // UNKNOWN
  
  p = &z[-1]; 
  assert(p == &y); // UNKNOWN

  p = &z[y];
  assert(p == &z[y]); // UNKNOWN

  return 0;
}
