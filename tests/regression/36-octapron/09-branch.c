// SKIP PARAM: --sets ana.activated[+] octApron
// Based on 01/35.
#include <assert.h>

void main() {
  int i;
  if (i - 1) {
    // only implies i - 1 != 0    (i != 1)
    // doesn't imply i - 1 == 1   (i == 2)
    // doesn't imply i - 1 != 1   (i != 2)
    assert(i == 2); // UNKNOWN!
  }
}
