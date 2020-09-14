// PARAM: --disable ana.int.interval --enable ana.int.def_exc --enable ana.int.enums
#include <assert.h>
#include <stdio.h>

int main(){
    unsigned int i;

    // With i = 7 the then-branch will be reached.
    // i = 7;
    unsigned int r = i * 1073741824u;
    if(i *  1073741824u == 3221225472u){
        printf("%u\n", i);
        assert(i == 3); // UNKNOWN
    }
    return 0;
}
