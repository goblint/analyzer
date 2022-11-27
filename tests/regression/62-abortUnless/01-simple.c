// PARAM: --set ana.activated[+] abortUnless
#include <goblint.h>

int zero() { return 1; }

void more_faux_abort(int cond) {
  if(!cond)
  {
    abort();
  }
  cond = zero();
}

void faux_abort(int cond) {
  if(cond)
  {
    abort();
  }
}

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

    int y;
    faux_abort(y == 8);
    __goblint_check(y==8); //UNKNOWN!

    int z;
    more_faux_abort(z == 8);
    __goblint_check(z==8); //UNKNOWN!
}
