// PARAM: --enable ana.int.interval --disable ana.int.def_exc --disable ana.int.enums
#include <assert.h>
#include <stdio.h>

int main(){
    unsigned int i = 3;

    // 3 * 2^30 == 3221225472u is outside of the range that Intervall32 can represent
    // Therefore, when trying to refine i, Base.invariant meets i -> [3;3] with i -> [(-2^31) / 2^30; ((2^31)-1) / 2^30] = [-2; 1]
    // We thus get i -> Bottom, and the code after the condition is considered unreachable
    if(i *  1073741824u == 3221225472u){
        printf("%u\n", i);
        assert(i == 3); // SUCCESS
    }
    assert(i == 3); // SUCCESS
    return 0;
}
