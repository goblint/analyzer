// PARAM: --enable ana.int.def_exc --disable ana.int.interval
#include<assert.h>
#include<stdio.h>
int main(){
    unsigned long top;
    printf("value to reach true branch: %ld\n", (-1 / (8 * sizeof(int))));
    if (top == (-1 / (8 * sizeof(int)))){ // parts of the expression evaluate to bottom and make Goblint crash
        assert(1);
    }
    return 0;
}
