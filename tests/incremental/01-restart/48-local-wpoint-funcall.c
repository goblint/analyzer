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
        assert(x == 0); // TODO (fail before, success after?)
    }

    assert(0); // TODO (fail before, nothing after)
    return 0;
}