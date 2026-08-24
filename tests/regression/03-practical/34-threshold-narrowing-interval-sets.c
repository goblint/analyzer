// PARAM: --enable ana.int.interval_set --enable ana.int.interval_threshold_widening --set ana.int.interval_threshold_widening_constants comparisons
#include <goblint.h>

int main() {
    int i;
    for(i = 0; i < 10 && i < 20; i += 3);
    __goblint_check(i <= 12);

    int j;
    for(j = 0; j > -10 && j > -20; j-= 3);
    __goblint_check(j >= -12);
    
}
