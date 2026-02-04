// SKIP TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra

// from: https://www.cs.cmu.edu/~hzarnani/fm-rg/cmuonly/podelski04complete.pdf, Example 2
// There is NO linear ranking function here (x doesn't decrease monotonously, e.g. (5, 0, 10, -10))

#include <stdlib.h>
int main() {
    int x;

    while (x >= 0) {
        x = -2 * x + 10;
    }
}
