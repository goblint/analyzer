// PARAM: --enable ana.int.interval_set --disable ana.int.def_exc --disable ana.int.enums
#include <goblint.h>

void main(){
  int n = 7;
  for (; n; n--) {
      __goblint_check(n==1); // UNKNOWN!
  }
  int i;
  if(i-1){
    __goblint_check(i==2); // UNKNOWN!
  }
  return;
}
