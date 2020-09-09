// PARAM: --enable ana.int.def_exc --disable ana.int.interval
#include<assert.h>
// Some unsigned long constant that is larger than 2^63-1
#define SIZE_MAX 18446744073709551615UL

int foo(){
    int top;
    int a = 3;
    if (top < SIZE_MAX / (8 * sizeof(int))){ // parts of the expression errorneously evaluate to bottom
        assert(1);
    }
    return 0;
}
