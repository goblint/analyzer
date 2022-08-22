//PARAM: --disable ana.int.def_exc --disable ana.int.enums --enable ana.int.interval

#include<assert.h>
#include<stdio.h>

int main(){
    int x = 2;
    int y = 3;
    int n = 0;
    int m = 24;
    int z;

    // logical and
    __goblint_check((x && y) == 1);

    z = x & y; // bitwise and
    __goblint_check(z == 2);

    // logical or
    __goblint_check((x || y) == 1);

    __goblint_check((x || 1) == 1);

    z = x | y; // bitwise or
    __goblint_check(z == 3);

    z = x ^ y; // bitwise xor
    __goblint_check(z == 1);

    // logical negation
    __goblint_check(!x == 0);

    z = !n;
    __goblint_check(z == 1);

    z = ~x; // bitwise negation
    __goblint_check(z == -3);

    z = x << y; // shift left
    __goblint_check(z == 16);

    z = m >> x; // shift right
    __goblint_check(z == 6);

    return 0;
}
