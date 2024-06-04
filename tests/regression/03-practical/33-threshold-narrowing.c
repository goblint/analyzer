// PARAM: --enable ana.int.interval --enable ana.int.interval_threshold_widening --set ana.int.interval_threshold_widening_constants comparisons
#include <goblint.h>

int main() {
    int i;
    for(i = 0; i < 10 && i < 20; i += 3);
    __goblint_check(i <= 12);
}
