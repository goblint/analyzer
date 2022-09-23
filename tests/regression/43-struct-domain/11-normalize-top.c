// PARAM: --set ana.base.structs.domain "sets"

#include<assert.h>

struct Pair {
    int first;
    int second;
};

void example1() {
    int a;
    int b;

    struct Pair pair;
    // Both first and second are uninitialized -> top

    if (a > 10) {
        pair.first = 10;
        pair.second = 20;
    }
    // We now might have two variants;
    // { first = top, second = top } and { first = 10, second = 20 }

    // Result should still be top since branch is not always taken!
    __goblint_check(pair.first == 10); // UNKNOWN!
    __goblint_check(pair.second == 20); // UNKNOWN!

    if (a > 10) {
        // The analysis currently cannot infer this, since a is not connected to struct
        __goblint_check(pair.first == 10); // TODO
        __goblint_check(pair.second == 20); // TODO
    }

    if (pair.first == 10) {
        __goblint_check(pair.first == 10); // This is known from the if statement refine

        // Since one variant is top, we still don't know what second is!
        __goblint_check(pair.second == 20); // UNKNOWN!
    }

    pair.first = a;
    pair.second = b;
    // Reset both to top, should be same state as before.
    __goblint_check(pair.first == 10); // UNKNOWN!
    __goblint_check(pair.second == 20); // UNKNOWN!

    pair.first = 10;
    pair.second = 20;
    // Set both to known state - should be working
    __goblint_check(pair.first == 10);
    __goblint_check(pair.second == 20);
}


int main() {
    example1();

    return 0;
}
