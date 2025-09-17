//SKIP PARAM: --set ana.activated[+] lin2vareq_p --set sem.int.signed_overflow assume_none 

int main() {
    int x, y, z;
    x = 3 * y + 1; // a
    z = 5 * x + 7; // b
    if (y < 14)
    {
        __goblint_check( x <= 42); //SUCCESS
        __goblint_check(y < 14); //SUCCESS
        __goblint_check(z != 500);  //SUCCESS
        __goblint_check(z != 14);  //SUCCESS // Because of eqution for z
        __goblint_check(z != 17);  //SUCCESS // Because of combination of equation for z and x 

    }
}