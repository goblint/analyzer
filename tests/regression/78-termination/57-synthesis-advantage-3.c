// SKIP TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra

// from: https://dl.acm.org/doi/pdf/10.1145/1941487.1941509, Figure 2
// Linear ranking function exists only if numbers are bounded, otherwise lexicographic needed

#include <stdlib.h>

int main() {
    int x, y;

    while (x > 0 && y > 0) {
        if (rand() == 0) {
            x = x - 1;
            y = rand();
        } else {
            y = y - 1;
        }
    }
}
