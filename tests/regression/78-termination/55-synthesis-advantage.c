// SKIP TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra

// Termination only provable by more complex algorithm?

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
