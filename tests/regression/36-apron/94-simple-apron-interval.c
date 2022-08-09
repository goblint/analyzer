// SKIP PARAM: --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain interval
// Example from https://www-apr.lip6.fr/~mine/publi/article-mine-HOSC06.pdf, adapted
#include <assert.h>

void main(void) {
  int X = 0;
  int N = rand();
  if(N < 0) { N = 0; }

  while(X < N) {
    X++;
  }

  __goblint_check(X-N == 0); //UNKNOWN
  __goblint_check(X == N); //UNKNOWN

  if(X == N) {
    N = 8;
  } else {
    N = 42;
  }
  __goblint_check(N == 8); // UNKNOWN
  __goblint_check(N >= 8);
  __goblint_check(N <= 42);
}
