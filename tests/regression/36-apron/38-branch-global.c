// SKIP PARAM: --set ana.activated[+] apron --set ana.path_sens[+] threadflag --disable ana.int.interval
#include <goblint.h>

int ARR_SIZE = 10;

int main() {
    int count = 0;
    while(count<ARR_SIZE) {
        __goblint_check(count >= 0);
        __goblint_check(count < ARR_SIZE);
        count++;
    }
    __goblint_check(count == ARR_SIZE); // TODO (requires threshold)
    return 0 ;
}
