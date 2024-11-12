//PARAM: --enable ana.int.bitfield  --set sem.int.signed_overflow assume_none --disable ana.int.def_exc --disable ana.int.enums
#include <goblint.h>

int main() {
    int x;

    if (x+1)

    return 0;
}