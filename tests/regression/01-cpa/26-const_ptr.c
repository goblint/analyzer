// SKIP!
#include <assert.h>
extern void f(int* const*);
int main()
{
  int n = 0;
  int* pn = &n;
  int* t = pn;
  f(&pn);
  __goblint_check(n); // UNKNOWN!
  __goblint_check(pn == t);
  return 0;
}
