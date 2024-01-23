// PARAM: --set asm_is_nop false

#include <goblint.h>

int main(void) {

    int v = 0;

    __asm__("nop": "=x"(v)); 

    __goblint_assert(v==0); //UNKNOWN
}