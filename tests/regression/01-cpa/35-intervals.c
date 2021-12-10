// PARAM: --enable ana.int.interval --disable ana.int.def_exc --disable ana.int.enums
#include <assert.h>

void main(){
  int n = 7;
  for (; n; n--) {
      assert(n==1); // UNKNOWN!
  }
  int i;
  if(i-1){
    assert(i==2); // UNKNOWN!
  }
  return;
}
