// PARAM: --enable ana.int.interval --set ana.base.arrays.domain unroll --set ana.base.arrays.unrolling-factor 2
#include <goblint.h>
int global;

int main(void)
{
    example1();
    example2();
    return 0;
}

// Simple example
void example1(void)
{
    int a[42];
    int i = 0;
    int top;

    while (i < 42) {
        a[i] = 0;
        __goblint_check(a[i] == 0); // UNKNOWN
        __goblint_check(a[0] == 0); // UNKNOWN
        __goblint_check(a[17] == 0);
        i++;
    }

    __goblint_check(a[0] == 0); // UNKNOWN
    __goblint_check(a[7] == 0);
    __goblint_check(a[41] == 0);
}


// Check that arrays of types different from int are handeled correctly
void example2() {
    // no char because char has unknown signedness (particularly, unsigned on arm64)
    signed char a[10];
    int n;
    __goblint_check(a[3] == 800); // FAIL (char cannot be 800)
    __goblint_check(a[3] == 127); // UNKNOWN!

    for(int i=0;i < 10; i++) {
        a[i] = 7;
    }

    a[3] = (signed char) n;
    __goblint_check(a[3] == 800); //FAIL
    __goblint_check(a[3] == 127); //UNKNOWN
    __goblint_check(a[3] == -128); //UNKNOWN
    __goblint_check(a[3] == -129); //FAIL
}
