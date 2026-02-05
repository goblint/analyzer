// SKIP TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra

// x is strictly monotonously decreasing, but the classical termination analysis doesn't work

#include <stdlib.h>

int main() {
    int x;

    while (x >= 2) {
        if (x == 42) {
            x--;
        } else {
            x = x / 2;
        }
    }
}
