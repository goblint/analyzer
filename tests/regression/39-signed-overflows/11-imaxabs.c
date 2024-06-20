//PARAM: --enable ana.int.interval --set ana.activated[+] tmpSpecial
#include<stdlib.h>
#include <stdint.h>
#include <inttypes.h>
int main() {
    int64_t data;
    if (data > (-0x7fffffffffffffff - 1))
    {
        if (imaxabs(data) < 100)
        {
            __goblint_check(data < 100); // TODO
            __goblint_check(-100 < data); // TODO
            int64_t result = data * data; // TODO NOWARN
        }

        if(imaxabs(data) <= 100)
        {
            __goblint_check(data <= 100); // TODO
            __goblint_check(-100 <= data); // TODO
            int64_t result = data * data; // TODO NOWARN
        }
    }
    return 8;
}
