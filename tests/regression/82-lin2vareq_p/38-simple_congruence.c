//SKIP PARAM: --set ana.activated[+] lin2vareq_p --set sem.int.signed_overflow assume_none 

int main() {
    int x, y, z;
    x = 3 * y + 1; // a
    z = 5 * x + 7; // b
    if (y < 14)
    {
        __goblint_check( x <= 42);
        __goblint_check(y < 14); // A
        __goblint_check(z != 500);  // B
        __goblint_check(z != 14);  // Because of eqution for z
        __goblint_check(z != 17);  // Because of combination of equation for z and x 

    }
}