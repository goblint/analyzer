// SKIP PARAM: --set ana.activated[+] lin2vareq

/**
 * This test shows an instance where MaySignedOverflow raised
 * an uncaught division by zero in the treatment of main's sole statement
 * 
 * Fixed in #1419
 */
int a;
char b;
int main()
{
     0 == a && 1 / b;
    __goblint_check(1); // (reachable)
    return 0;
}
