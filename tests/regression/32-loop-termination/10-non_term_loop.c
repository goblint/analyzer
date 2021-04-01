// PARAM: --set ana.activated[+] "'octApron'" --enable dbg.debug --enable ana.int.interval

int add (int x, int y) {
    return x + y;
}

int main(void) {
    int x, y, r;
    x = 5;
    assert(x == 3); // FAIL
    assert(x == 5); // SUCCESS
    y = 3;
    r = x + y;
    while (x != y) {
        assert(r > 0);  // base analysis doens't know this but octApron does
        if (y > x) 
            y = y - x;
        else 
            x = x - y;
        assert(r > x + y);  // base analysis doens't know this but octApron does
        x = add(x, y+1);
        r = x + y; 
    }
    return 0;
}