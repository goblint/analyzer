// PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.def_exc --disable exp.fast_global_inits --enable exp.partition-arrays.enabled  --set ana.activated "['base','expRelation']"
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
