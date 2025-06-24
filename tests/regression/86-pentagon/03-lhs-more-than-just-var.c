// PARAM: --set "ana.activated[+]" pentagon

// Taken from tests/regression/66-interval-set-one/73-intervals.c
#include <goblint.h>

int main(void) {
    int x;
    int one = 1;
    int two = 2;
    if(x + one == two){
        __goblint_check(x == one); // UNKNOWN
    }
}