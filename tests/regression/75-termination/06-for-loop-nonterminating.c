// NONTERM PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main() {
    for (;;) {
        printf("This loop does not terminate.\n");
    }

    return 0;
}
