// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --disable warn.info --set sem.int.signed_overflow assume_none
int main() {
    int *p = malloc(2 * sizeof(int));
    int *q = p;
    int x;

    if (x) {
        q++;
        q++;
        q++;
        x = *q;  //WARN
    }

    x = *q;  //WARN
    return 0;
}
