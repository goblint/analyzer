// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --set sem.int.signed_overflow assume_none --disable ana.int.interval
// copied from signed-overflows/intervals for apron
#include <assert.h>

int main(void) {
    int x = 0;
    while(x != 42) {
        x++;
        __goblint_check(x >= 1);
    }

}
