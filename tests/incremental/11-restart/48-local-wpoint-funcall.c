#include <assert.h>

int f(int x) {
    return 1;
}

int main() {
    int x = 0;
    int y;
    while (x < 10) {
        y = f(x);
        x = x + y;
        __goblint_check(x == 0); // FAIL before, success after
    }

    __goblint_check(0); // FAIL before, nowarn after
    return 0;
}