// PARAM: --enable ana.int.interval --set ana.base.arrays.domain partitioned  --set ana.base.partition-arrays.keep-expr "last"
#include <assert.h>

int main(void) {
    int a[4];
    int b[4];

    a[0] = 42;
    a[1] = 42;
    a[2] = 42;
    a[3] = 42;

    __goblint_check(a[0] == 42);
    __goblint_check(a[1] == 42);
    __goblint_check(a[2] == 42);
    __goblint_check(a[3] == 42);

    int *ptr = &b;
    *ptr = 1; ptr++;
    *ptr = 1; ptr++;
    *ptr = 1; ptr++;
    *ptr = 1; ptr++;

    __goblint_check(b[0] == 1);
    __goblint_check(b[1] == 1);
    __goblint_check(b[2] == 1);
    __goblint_check(b[3] == 1);
}
