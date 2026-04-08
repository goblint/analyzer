// SKIP CRAM PARAM: --set ana.activated[+] apron --enable ana.int.interval
// Simplified from 36-apron/01-octagon_simple
#include <goblint.h>

void main(void) {
  int X = 0;
  int Y = 0;
  int N = rand();
  if(N < 0) { N = 0; }

  while(X < N) {
    X++;
  }
}
