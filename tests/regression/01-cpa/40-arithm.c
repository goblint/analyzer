// PARAM: --enable ana.int.def_exc --disable ana.int.interval
#include<stdio.h>
#include <goblint.h>
// 2 ^ 30
#define MULT 1073741824

int main(){
    unsigned int top;
    unsigned int result;
    // top = 7;
    if(top != 3){
        result = top * MULT;
        // if top == 7 then we have (2 + 1) * 2^30 == (4 + 2 + 1) * 2^30  (mod 2^32)
        __goblint_check(result != 3221225472); // UNKNOWN!
        printf("%u\n", result);
    }
    return result;
}
