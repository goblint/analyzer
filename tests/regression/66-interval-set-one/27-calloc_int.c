// PARAM: --set ana.int.interval_set true --set ana.base.arrays.domain partitioned
#include<stdlib.h>
#include <goblint.h>

int main(void) {
    int *r = calloc(1,sizeof(int));

    r[0] = 0;

    __goblint_check(r[0] != 5);
    __goblint_check(r[0] == 0);

    r[0] = 5;

    __goblint_check(r[0] == 5); //UNKNOWN
    __goblint_check(r[0] != 0); //UNKNOWN
    __goblint_check(r[0] != -10);
    __goblint_check(r[0] != 100);
}
