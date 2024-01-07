//SKIP PARAM: --set ana.activated[+] lin2vareq --set sem.int.signed_overflow assume_none
#include <stdio.h>
#include <goblint.h>

int main() {
    int x = 1;
    int y = 1;
    int z = 1;

    for (int i = 1; i <= 3; i++) {
        x = x * i;
        y = y + x;
        z = z + (y - x); 
    }

    __goblint_check(x == 6); //UNKNOWN!
    __goblint_check(z != 1); //UNKNOWN!

    return 0;
}
