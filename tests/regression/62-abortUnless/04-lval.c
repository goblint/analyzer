// PARAM: --set ana.activated[+] abortUnless
#include <goblint.h>

int assume_abort_if_not(int cond) {
  if(!cond)
  {
    abort();
  }
  return 42;
}

int main(void)
{
    int x, y;
    y = assume_abort_if_not(x == 8);
    __goblint_check(x==8);
    __goblint_check(y==42);
}
