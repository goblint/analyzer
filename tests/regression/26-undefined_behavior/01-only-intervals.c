// PARAM: --set solver td3 --enable ana.int.interval --disable ana.int.def_exc --set ana.activated "['base','threadid','threadflag','escape','expRelation','mallocWrapper','assert']" --set ana.base.privatization none
#include <assert.h>

int main() {
  for(int i=2; i < 42; i++) {
    int x = i==2; // NOWARN
    assert(1);
  }
}
