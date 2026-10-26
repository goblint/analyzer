// PARAM: --set ana.activated[+] subpoly --set ana.activated[+] abortUnless --enable ana.int.interval --set sem.int.signed_overflow assume_none

#include <goblint.h>

extern void abort(void);

static void assume_valid(int cond) {
    if (!cond) abort();
}

int main(void) {
    int x = 50000;
    __goblint_check(x == 50000); // SUCCESS
    // Under assume_none, x*x has no defined signed-int result. The shared
    // relational converter queries x*x+x while converting the outer sum
    // in the assertion event emitted by abortUnless after this call.
    // This speculative query must not leak ArithmeticOnIntegerBot, and the
    // ordinary expression evaluation must still report the overflow.
    assume_valid((x * x + x) + x == 0); // WARN NOCRASH
    return 0;
}
