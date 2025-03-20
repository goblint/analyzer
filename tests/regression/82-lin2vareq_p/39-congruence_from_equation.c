//SKIP PARAM: --set ana.activated[+] lin2vareq_p --set sem.int.signed_overflow assume_none 

int main() {
    int x, y, z;
    if (4 * x == 3 * y + 1) {
        __goblint_check( y % 4 == 1);
        __goblint_check( x % 3 == 1);

    }
}