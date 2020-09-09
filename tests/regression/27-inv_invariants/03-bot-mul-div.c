// PARAM: --enable ana.int.def_exc --disable ana.int.interval
#include<assert.h>

int foo(){
    int top;
    int a = 3;
    if (top < -1 / (8 * sizeof(int))){ // parts of the expression errorneously evaluate to bottom
        assert(1);
    }
    return 0;
}
