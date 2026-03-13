// SKIP TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra

// from: https://www.cs.cmu.edu/~hzarnani/fm-rg/cmuonly/podelski04complete.pdf, Example 1

#include <stdlib.h>
int main() {
    int i, j, nat, pos;
    __goblint_assume(nat >= 0);
    __goblint_assume(pos >= 1);

    while (i - j >= 1) {
        j = j + pos;
        i = i - nat;
    }
}