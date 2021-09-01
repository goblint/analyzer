// SKIP PARAM: --set ana.activated[+] apron --enable ana.int.interval --set sem.int.signed_overflow assume_none

int main(void) {
    int x, y, r;
    x = 5;
    y = 3;
    r = x + y;
    while (x != y) {
        assert(r > 0);
        if (y > x)
            y = y - x;
        else
            x = x - y;
        assert(r > x + y);
        r = x + y;
    }
    return 0;
}