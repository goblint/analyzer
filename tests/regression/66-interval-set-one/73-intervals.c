// PARAM: --set sem.int.signed_overflow assume_none --enable ana.int.interval_set --disable ana.int.def_exc
#include <goblint.h>

int main(void) {
    int x = 0;
    while(x != 42) {
        x++;
        __goblint_check(x >= 1);
    }

}
