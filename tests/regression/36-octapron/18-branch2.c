// SKIP PARAM: --sets ana.activated[+] octApron
// Based on 36/09.
#include <assert.h>

void main() {
  int i;
  if (i) { // same as i != 0
    // only implies i != 0
    // doesn't imply i > 0
    // doesn't imply i >= 1
    assert(i >= 1); // UNKNOWN!
  }
  else {
    // implies i == 0
    // doesn't imply i < 0
    assert(i == 0);
    assert(i < 0); // FAIL
  }
}
