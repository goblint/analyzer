// PARAM: --set sem.int.signed_overflow assume_none --enable ana.int.interval --disable ana.int.def_exc
int main(void) {
    int x = 0;
    while(x != 42) {
        x++;
        assert(x >= 1);
    }

}
