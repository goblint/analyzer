// PARAM: --enable ana.int.interval
#include <limits.h>

int main() {
    int bad = INT_MIN / -1; // WARN (overflow)
    int x, y;
    bad = x / y; // WARN (div by zero and overflow, distinguished in cram test)
    if (y != 0) {
        bad = x / y; // WARN (overflow)
    }
    return 0;
}
