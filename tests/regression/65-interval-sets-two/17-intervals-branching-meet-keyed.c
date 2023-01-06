// PARAM: --enable ana.int.interval_set --set ana.base.structs.domain "keyed"

#include <goblint.h>

struct Pair {
    int first;
    int second;
};

void example1() {
    int a;
    int b;

    struct Pair pair;

    if (a) {
        pair.first = 10;
        pair.second = 20;
    } else {
        pair.first = 20;
        pair.second = 30;
    }

    if (pair.first == 15) {
        // This should be unreachable!
        b = 0; // This line is not dead if we --disable ana.base.structs.meet-condition
    } else if (pair.first == 10) {
        __goblint_check(pair.second == 20);
        b = 1;
    } else if (pair.first == 20) {
        __goblint_check(pair.second == 30);
        b = 1;
    }
    __goblint_check(b == 1);
}


int main() {
    example1();

    return 0;
}
