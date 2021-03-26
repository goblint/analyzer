// PARAM: --set ana.activated[+] "'octApron'" --enable dbg.debug --enable ana.int.interval

int main() {
    int i = 0;
    int x = 10;
    int y = 5;
    int r = x + y;
    while (i < 5) {
        assert(r > 0); // UNKNOWN // base analysis doens't know this but octApron does
        i++;
        if (x > y) x -= y;
        assert(r > x + y - i); // UNKNOWN // base analysis doens't know this but octApron does
        r = x + y - i;
    }
    return 0;
}