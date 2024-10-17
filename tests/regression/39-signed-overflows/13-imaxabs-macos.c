//PARAM: --enable ana.int.interval --set ana.activated[+] tmpSpecial
// 39-signed-overflows/11-imaxabs, but with long long as int64_t instead (https://github.com/goblint/analyzer/pull/1519#issuecomment-2417032186).
#include<stdlib.h>
#include <stdint.h>
#include <inttypes.h>
int main() {
    long long data;
    if (data > (-0x7fffffffffffffff - 1))
    {
        if (imaxabs(data) < 100)
        {
            __goblint_check(data < 100);
            __goblint_check(-100 < data);
            long long result = data * data; // NOWARN
        }

        if(imaxabs(data) <= 100)
        {
            __goblint_check(data <= 100);
            __goblint_check(-100 <= data);
            long long result = data * data; // NOWARN
        }
    }
    return 8;
}
