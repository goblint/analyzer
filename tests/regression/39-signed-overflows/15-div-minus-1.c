// PARAM: --enable ana.int.interval
#include <limits.h>

int main() {
    int bad = INT_MIN / -1; // WARN
    int x, y;
    if (y != 0) {
        bad = x / y; // WARN
    }
    return 0;
}
