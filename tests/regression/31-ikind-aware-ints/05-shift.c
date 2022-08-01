// PARAM: --enable ana.int.interval --set ana.base.arrays.domain partitioned --set ana.activated "['base', 'mallocWrapper','assert']" --set ana.base.privatization none
int main(void) {
    // Shifting by a negative number is UB, but we should still not crash on it, but go to top instead
    int v = -1;
    int r = 17;
    int u = r >> v;
}
