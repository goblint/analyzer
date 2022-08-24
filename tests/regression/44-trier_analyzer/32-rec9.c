#include <assert.h>

void p (int *x) {
  int y;
  int *z;
  z = &y;
  p(z);
}

main() {
  int z;
  int ind = 0;
  if(z) {
    p(&z);
    // p does not return
    ind = 1;
  }

  __goblint_check(ind == 0);
}
