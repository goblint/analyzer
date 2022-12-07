// SKIP PARAM: --set ana.activated[+] apron --enable ana.int.interval
#include <goblint.h>

extern int __VERIFIER_nondet_int();

int main(void) {
    f1();
}

int f1() {
    int one = __VERIFIER_nondet_int();
    int two = __VERIFIER_nondet_int();

    int x = __VERIFIER_nondet_int();

    one = two;

    __goblint_check(one - two == 0);
    __goblint_check(one == two);
    x = f2(one,two);
    __goblint_check(one - two == 0);
    __goblint_check(one == two);
    __goblint_check(x == 48);
}

int f2(int a, int b) {
    __goblint_check(a-b == 0);
    __goblint_check(a == b);

    return 48;
}
