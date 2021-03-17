// PARAM: --enable dbg.debug --enable ana.int.interval

int main() {
    int t1 = 0, t2 = 0;
    int i;
    for (i = 0; i < 5; i++) {
        t1++;
        t2--;
    }
    assert(t1 == i); //SUCCESS!

    t1 = 5;
    t2 = 5;
    for (i = 5; i > 0; i--) {
        t1++;
        t2--;
    }
    assert(t2 == i); //SUCCESS! 

    t1 = 0;
    t2 = 0;
    for (i = 0; i < 5; i--) {
        t1++;
        t2--;
    }
    assert(t1 == i); //FAIL!
    return 0;
}