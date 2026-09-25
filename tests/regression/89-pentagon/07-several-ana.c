// SKIP PARAM: --enable ana.int.interval --set ana.activated[+] pentagon

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
    // is dead code but if that is detected or not depends on what we do in branch
    // currently we can't detect this
    N = 42;
  }
}