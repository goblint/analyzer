// SKIP!
#include <assert.h>
extern void f(int* const*);
int main()
{
  int n = 0;
  int* pn = &n;
  int* t = pn;
  f(&pn);
  assert(n); // UNKNOWN!
  assert(pn == t);
  return 0;
}
