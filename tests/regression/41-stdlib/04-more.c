// PARAM: --set pre.cppflags[+] -DGOBLINT_NO_QSORT
#include<goblint.h>

int g = 8;

int main() {

    int r = __builtin_popcount(5u);
    int r = __builtin_popcountl(5ul);
    int r = __builtin_popcountll(5ul);

    // Should not be invalidated
    __goblint_check(g == 8);
}
