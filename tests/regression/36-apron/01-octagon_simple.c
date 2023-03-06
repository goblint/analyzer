// SKIP PARAM: --set ana.activated[+] apron --enable ana.int.interval
// Example from https://www-apr.lip6.fr/~mine/publi/article-mine-HOSC06.pdf
#include <goblint.h>

void main(void) {
  int X = 0;
  int N = rand();
  if(N < 0) { N = 0; }

  while(X < N) {
    X++;
  }

  __goblint_check(X-N == 0);
  __goblint_check(X == N);

  if(X == N) {
    N = 8;
  } else {
    // dead code
    N = 42;
  }

  __goblint_check(N == 8);
  two();
}

void two() {
  int X ;
  int N ;
  int tmp ;

  X = 0;
  tmp = rand();
  N = tmp;


  if (N < 0) {
    N = 0;
  }

  __goblint_check(X <= N);

  while (1) {
    while_continue: /* CIL Label */ ;
    if (! (X < N)) {
      goto while_break;
    }
    X ++;
  }
  while_break: /* CIL Label */ ;

  __goblint_check(X - N == 0);
  __goblint_check(X == N);
}
