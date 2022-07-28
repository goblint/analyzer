// PARAM: --set ana.int.interval true --set ana.base.arrays.domain partitioned

#include<stdlib.h>
#include<assert.h>

int main(void) {
    int (*r)[5] = calloc(2, sizeof(int[5]));
    r[0][1] = 3;
    int* z = &r[0][1];

    __goblint_check(*z == 3); //UNKNOWN
}
