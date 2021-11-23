// PARAM: --enable ana.int.interval --set sem.int.signed_overflow assume_none
#include <assert.h>

int empty() {
  return -1; // return shouldn't cast to void* generally, but just for thread return
}

int main(void) {
  if (!empty()==-1) { // if -1 is cast to void*, it makes both branches dead!
    assert(1); // NOWARN (unreachable)
  }

  assert(1); // reachable
  return 0;
}
