// PARAM: --set asm_is_nop false

#include <goblint.h>

int main(void) {

    int r;
    int v;

    __asm__ goto ("nop": : : : test);

    v = 0;

    test:

    __goblint_assert(v == 0); //UNKNOWN

}