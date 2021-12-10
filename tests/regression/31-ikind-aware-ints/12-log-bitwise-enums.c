//PARAM: --disable ana.int.def_exc --enable ana.int.enums --disable ana.int.interval

#include<assert.h>
#include<stdio.h>

int main(){
    int x = 2;
    int y = 3;
    int n = 0;
    int m = 24;
    int z;

    // logical and
    assert((x && y) == 1);

    z = x & y; // bitwise and
    assert(z == 2);

    // logical or
    assert((x || y) == 1);

    assert((x || 1) == 1);

    z = x | y; // bitwise or
    assert(z == 3);

    z = x ^ y; // bitwise xor
    assert(z == 1);

    // logical negation
    assert(!x == 0);

    z = !n;
    assert(z == 1);

    z = ~x; // bitwise negation
    assert(z == -3);

    z = x << y; // shift left
    assert(z == 16); //UNKNOWN

    z = m >> x; // shift right
    assert(z == 6); //UNKNOWN

    int one = 1;
    z = one / 10;
    assert(z == 0);

    return 0;
}
