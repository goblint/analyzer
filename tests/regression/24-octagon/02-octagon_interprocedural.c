// SKIP PARAM: --enable ana.int.interval --set ana.base.arrays.domain partitioned --set ana.activated[+] apron
#include <assert.h>

int main(void) {
    f1();
}

int f1() {
    int one;
    int two;

    int x;

    one = two;

    __goblint_check(one - two == 0);
    x = f2(one,two);
    __goblint_check(one - two == 0);
    __goblint_check(x == 48);
}

int f2(int a, int b) {
    __goblint_check(a-b == 0);

    return 48;
}
