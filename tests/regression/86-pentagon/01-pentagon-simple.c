// PARAM: --set "ana.activated[+]" pentagon

// Run with ./goblint tests/regression/86-pentagon/01-pentagon-simple.c --conf ./conf/empty.json

#include <goblint.h>

void main(void) {
  int x;
  
  if (x <= 0) {
    x = -x;
  }

__goblint_check(x >= 0);

}
