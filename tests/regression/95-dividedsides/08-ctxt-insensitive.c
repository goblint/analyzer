// PARAM: --set ana.context.callString_length 0 --set "ana.activated[+]" call_string --set ana.ctx_sens "['call_string']" --enable ana.int.interval_set --enable solvers.td3.narrow-sides.enabled --enable solvers.td3.narrow-sides.stable

int g(int i) {
    if (i == 0) {
        return 0;
    }
    if (i > 0) {
        return g(i - 1);
    }
    return -1;
}

int main(void)
{
    __goblint_check(g(10) != -1);
    return 0;
}
