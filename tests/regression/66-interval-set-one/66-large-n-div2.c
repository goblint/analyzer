//PARAM: --enable ana.int.interval_set --enable ana.int.def_exc
#include <goblint.h>

int main(){
    int top;
    // 2^33
    long long x = 8589934592l;
    // 2^31 - 1
    long long y = 2147483647;

    if(top) {
        x = x - 1;
    }

    long long z = x/y;

    if(z == 4){
        // Should be reachable
        __goblint_check(1);
    }

    __goblint_check(z == 4);
}
