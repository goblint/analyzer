// PARAM: --set solver td3 --enable ana.int.interval --set ana.base.arrays.domain partitioned  --set ana.base.partition-arrays.keep-expr "last" --set ana.activated "['base','threadid','threadflag','escape','expRelation','mallocWrapper']" --set ana.base.privatization none
#include <assert.h>

int main(void) {
    int a[4];
    int b[4];

    a[0] = 42;
    a[1] = 42;
    a[2] = 42;
    a[3] = 42;

    assert(a[0] == 42);
    assert(a[1] == 42);
    assert(a[2] == 42);
    assert(a[3] == 42);

    int *ptr = &b;
    *ptr = 1; ptr++;
    *ptr = 1; ptr++;
    *ptr = 1; ptr++;
    *ptr = 1; ptr++;

    assert(b[0] == 1);
    assert(b[1] == 1);
    assert(b[2] == 1);
    assert(b[3] == 1);
}
