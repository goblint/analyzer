// PARAM: --enable ana.int.interval  --set ana.base.partition-arrays.keep-expr last --set ana.base.arrays.domain partitioned
#include <assert.h>

void main(void) {
  example1();
}

void example1(void) {
    int a[42];
    a[40] = 2;
    int i = 0;

    while(i < 41) {
        a[i] = 0;
        i++;
    }

    __goblint_check(a[2] == 0);
    __goblint_check(a[3] == 0);
}
