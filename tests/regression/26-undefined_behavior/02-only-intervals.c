// PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.def_exc --set ana.activated "['base','expRelation']"
#include <assert.h>

int main() {
  for(int i=2; i < 42; i++) {
    int x = i==2; // NOWARN
    assert(1);
  }
}
