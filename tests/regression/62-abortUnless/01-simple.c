// PARAM: --set ana.activated[+] abortUnless
#include <goblint.h>

void assume_abort_if_not(int cond) {
  if(!cond)
  {
    abort();
  }
}

int main(void)
{
    int x;
    assume_abort_if_not(x == 8);

    __goblint_check(x==8);
}
