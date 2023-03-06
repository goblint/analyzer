// PARAM: --set ana.activated[+] abortUnless
#include <goblint.h>

int zero(int cond) {
  return 0;
}

void assume_abort_if_not(int cond) {
  if (cond) {
    cond = zero(cond);
  }
  else {
    abort();
  }
}

int main(void)
{
  int x;
  assume_abort_if_not(x == 8);
  __goblint_check(x==8); // UNKNOWN!
}
