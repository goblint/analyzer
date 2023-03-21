// PARAM: --enable ana.int.interval_set --disable ana.int.def_exc
#include <goblint.h>
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
