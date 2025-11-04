// PARAM: --set ana.context.callString_length 0 --set "ana.activated[+]" call_string --set ana.ctx_sens "['call_string']" --enable ana.int.interval_set --enable solvers.td3.narrow-globs.enabled
#include <goblint.h>

int fac(int i) {
    if (i > 0) {
        return fac(i - 1) * i;
    }
    __goblint_check(i == 0);
    return 1;
}

int main(void)
{
    fac(10);
    return 0;
}
