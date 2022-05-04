// PARAM: --set ana.int.interval true --set ana.base.arrays.domain partitioned

#include<stdlib.h>
#include<assert.h>

int main(void) {
    int *ro = calloc(2,sizeof(int));
    assert(ro[0] == 0);
    assert(ro[1] == 0);

    ro[0] = 3;
    assert(ro[1] != 3); //UNKNOWN
}
