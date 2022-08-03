// PARAM: --enable ana.int.interval --disable ana.int.def_exc
#include <assert.h>

int main() {
  for(int i=2; i < 42; i++) {
    int x = i==2; // NOWARN
    __goblint_check(1);
  }
}
