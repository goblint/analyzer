// PARAM: --enable ana.int.interval --set exp.unrolling-factor 5 --set ana.base.arrays.domain unroll --set ana.base.arrays.unrolling-factor 5
// Simple example
#include <assert.h>

void main(void)
{
    int a[5];
    int i = 0;

    while (i < 5) {
        a[i] = i;
        i++;
    }

    __goblint_check(a[0] == 0);
    __goblint_check(a[3] == 3);
}
