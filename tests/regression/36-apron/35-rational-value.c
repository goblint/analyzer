// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag
#include <assert.h>

void main() {
  int x, a = 0;
  if (x > -1000 && x < 1000) { // avoid under-/overflow
    if (2 * x == 3) { // apron excludes true branch
      a = 1;
    }
    assert(a == 0);
  }
}
