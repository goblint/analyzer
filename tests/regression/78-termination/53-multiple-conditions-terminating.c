// SKIP TERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
// From #1725: CIL lowers && with a goto into the else branch, which is not upjumping
int main() {
    int c, x, y;
    c = 0;
    if (y > 46340) return 0;
    while ((x > 1) && (x < y)) {
        x = x*x;
        c = c + 1;
    }
    return 0;
}
