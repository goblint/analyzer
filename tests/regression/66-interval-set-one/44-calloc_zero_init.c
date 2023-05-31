// PARAM: --set ana.int.interval_set true --set ana.base.arrays.domain partitioned

#include<stdlib.h>
#include <goblint.h>

int main(void) {
    int *ro = calloc(2,sizeof(int));
    __goblint_check(ro[0] == 0);
    __goblint_check(ro[1] == 0);

    ro[0] = 3;
    __goblint_check(ro[1] != 3); //UNKNOWN
}
