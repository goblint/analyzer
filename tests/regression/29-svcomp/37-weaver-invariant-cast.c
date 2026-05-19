// PARAM: --enable ana.int.interval --set ana.activated[+] abortUnless
#include <goblint.h>

extern void abort(void);
void assume_abort_if_not(int cond) {
  if(!cond) {abort();}
}

int main() {
  int n_update;

  assume_abort_if_not(n_update >= 0);
  assume_abort_if_not(n_update <= ( 4294967295UL /  sizeof (char)));

  __goblint_check(1); // reachable
  return 0;
}
