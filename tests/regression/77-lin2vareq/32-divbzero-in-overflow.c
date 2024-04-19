// SKIP PARAM: --set ana.activated[+] lin2vareq

/**
 * This test shows an instance where MaySignedOverflow raised
 * an uncaught division by zero in the treatment of c's body
 * 
 * Fixed in #1419
 */
int a;
char b;
int c() { 0 == a && 1 / b; }
void d(int (*func)()) { func(); }
int main()
{
    d(c);
    __goblint_check(1); // (reachable)
    return 0;
}
