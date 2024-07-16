//PARAM: --enable annotation.int.enabled --set ana.int.refinement fixpoint

#include <goblint.h>
#include<stdio.h>

int main() __attribute__((goblint_precision("no-def_exc","enums")));

int main() {
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
    __goblint_check(z == 16); //UNKNOWN

    z = m >> x; // shift right
    __goblint_check(z == 6); //UNKNOWN

    int one = 1;
    z = one / 10;
    __goblint_check(z == 0);

    return 0;
}
