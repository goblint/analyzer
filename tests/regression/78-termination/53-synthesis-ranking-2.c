// SKIP TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra

// from: https://www.cs.unipr.it/~zaffanella/Papers/PDF/Q498.pdf, Example 4.6

#include <stdlib.h>

int main() {
    int x1;
    int x2 = 0;

    while (x1 >= 2) {
        x1 = x1 / 2;
        x2++;
    }
}
