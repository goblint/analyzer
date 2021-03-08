// PARAM: --set ana.activated[+] "'octApron'" --enable dbg.debug --enable ana.int.interval

int main(void) {
    int x, y, r;
    x = 5;
    assert(x == 3); // FAIL
    assert(x == 5); // SUCCESS
    y = 3;
    r = x + y;
    while (x != y) {
        assert(r > 0); // UNKNOWN // base analysis doens't know this but octApron does
        if (y > x) 
            y = y - x;
        else 
            x = x - y;
        assert(r > x + y); // UNKNOWN // base analysis doens't know this but octApron does
        r = x + y; 
    }
    return 0;
}