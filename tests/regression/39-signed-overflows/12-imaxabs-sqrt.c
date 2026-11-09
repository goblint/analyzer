//PARAM: --enable ana.int.interval --enable ana.float.interval --enable ana.float.evaluate_math_functions --set ana.activated[+] tmpSpecial
#include<math.h>
#include <stdint.h>
#include <inttypes.h>
int main() {
    int64_t data;
    if (data > (-0x7fffffffffffffff - 1) && imaxabs((intmax_t)data) <= sqrtl(0x7fffffffffffffffLL))
    {
        int64_t result = data * data; // NOWARN
    }
    return 8;
}
