// PARAM: --set solver td3 --enable ana.int.interval --disable ana.int.def_exc
#include <assert.h>
#include <stdio.h>
int main(){
    int i;
    if(i<0){
      assert(i<0);
    } else {
      assert(i>=0);
    }
    return 0;
}
