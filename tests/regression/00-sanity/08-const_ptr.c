#include <assert.h>
extern void f(int* const*);
int main()
{
  int n = 0;
  int* pn = &n;
  int* t = pn;
  f(&pn);
  assert_unknown(n);
  assert(pn == t);
  return 0;
}
