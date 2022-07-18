#include "assert.h"
int main() {
    int x = -1;
    int m = x % 5;
    int r = x /5;
    assert(m == -1);
    assert(r == 0);

    x = 1;
    m = x%-5;
    r = x/-5;
    assert(m == 1);
    assert(r == 0);

    x = -1;
    m = x%-5;
    r = x/-5;
    assert(m == -1);
    assert(r == 0);
}
