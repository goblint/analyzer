// PARAM: --enable ana.int.interval
int main(void) {
    // NOCRASH: Shifting by a negative number is UB, but we should still not crash on it, but go to top instead
    int v = -1;
    int r = 17;
    int u = r >> v;
}
