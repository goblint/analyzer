// PARAM: --enable ana.int.interval_set --set sem.int.signed_overflow assume_wraparound
#include <goblint.h>
#include <limits.h>

int main() {

    char i;

    if (i < 126) {
        i = 126;
    }

    i++;
    
    __goblint_check(i != 0);

    return 0;
}