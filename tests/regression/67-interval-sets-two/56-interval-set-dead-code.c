// PARAM: --enable ana.int.interval_set
#include <goblint.h>
#include <limits.h>

int main() {

    int i;

    if (i > 5 && i < 10) {
        i = 1;
    }
    if (i == 7) {
        i = INT_MAX;
        i += 1;
    }

    return 0;
}