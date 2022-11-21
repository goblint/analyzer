// PARAM: --set ana.activated[+] abortUnless
#include <goblint.h>

int zero(int cond) {
  return 0;
}

int main(void)
{
  int x;

  if (x) {
    // Assignment to x has already happened before the event is processsed
    // To solve this make calls that have an RHS ineligible
    x = zero(x);
  }

  // reachable
  __goblint_check(1);
}
