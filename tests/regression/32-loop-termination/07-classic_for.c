// PARAM: --set ana.activated "['base','threadid','threadflag','term','mallocWrapper', 'poly', 'octy']" --enable dbg.debug --enable ana.int.interval --sets solver slr3

int main() {
    int a[55];
    for (int i = 0; i < 55; i++) {
        a[i] = i;
    }
    return 0;
}