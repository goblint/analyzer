// PARAM: --enable ana.int.interval --disable ana.int.def_exc
#include <assert.h>
#include <stdio.h>
int main(){
    int i;
    if(i<0){
      __goblint_check(i<0);
    } else {
      __goblint_check(i>=0);
    }
    return 0;
}
