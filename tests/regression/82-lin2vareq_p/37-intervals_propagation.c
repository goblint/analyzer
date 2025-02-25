//SKIP PARAM: --set ana.activated[+] lin2vareq_p --set sem.int.signed_overflow assume_none 

int main() {
    int x, y, z;
    x = 3*y + 1; // a
    z = 5*x + 7; // b
    if (x>0) {
    __goblint_check( x >  0 );
    __goblint_check( y > -1 ); // A
    __goblint_check( z >  7 ); // B
    }
}