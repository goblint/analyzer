// SKIP PARAM: --set ana.activated[+] subpoly --set sem.int.signed_overflow assume_none
// NOCRASH

#include <goblint.h>

int main(void) {
    int a;
    int b;
    int x = 1;
    int y = 2;

    __goblint_assume(a + b < 2);

    return x + y;
}
