// PARAM: --set solvers.td3.widen_gas 11  --enable ana.int.interval --enable exp.earlyglobs
#include <goblint.h>
int g = 0;

int main () {
    int i = 0;
    // i is widened 11 times:
    // [0, 0] -> [0, 1] -> ... -> [0, 10]
    loop:
    if(i > 11) {
        g = 42;
    }

    // Exit with '==' condition to prevent narrowing from
    // regaining any meaningful information through the loop body.
    if (i == 10)
        goto end;
    i++;
    goto loop;
    end:
    __goblint_check(g != 42);
}
