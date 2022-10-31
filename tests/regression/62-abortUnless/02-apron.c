// PARAM: --set ana.activated[+] abortUnless --set ana.activated[+] apron
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
    int x, x1;
    assume_abort_if_not(x <= x1);
    __goblint_check(x <= x1);

    int y, y1;
    faux_abort(y <= y1);
    __goblint_check(y <= y1); //UNKNOWN!

    int z, z1;
    more_faux_abort(z <= z1);
    __goblint_check(z <= z1); //UNKNOWN!
}
