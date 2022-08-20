// PARAM: --set ana.base.arrays.unrolling-factor 2  --enable annotation.array
#include <assert.h>

void main(void) {
    unrollToTrivial();
}

int unrollToTrivial(void)
{
    int a[42] __attribute__((goblint_array_domain("unroll")));

    a[0] = 0;
    a[1] = 1;
    a[2] = 2;
    a[3] = 3;

    trivial(a);

    assert(a[0] == 7); //UNKNOWN

    //here unroll is used again
    a[0] = 0;
    a[1] = 1;
    a[2] = 2;
    a[3] = 3;

    assert(a[1] == 1);
    assert(a[2] == 2); //UNKNOWN

}

void trivial(int* a __attribute__((goblint_array_domain("trivial")))){

    assert(a[0] == 0); //UNKNOWN
    assert(a[1] == 1); //UNKNOWN
    assert(a[2] == 2); //UNKNOWN
    assert(a[3] == 3); //UNKNOWN

    a[0] = 7;
}
 