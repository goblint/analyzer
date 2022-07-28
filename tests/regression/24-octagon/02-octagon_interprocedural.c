// SKIP PARAM: --set solver td3 --enable ana.int.interval --set ana.base.arrays.domain partitioned  --set ana.activated "['base','threadid','threadflag','expRelation','apron','mallocWrapper']" --set ana.base.privatization none
#include <assert.h>

int main(void) {
    f1();
}

int f1() {
    int one;
    int two;

    int x;

    one = two;

    assert(one - two == 0);
    x = f2(one,two);
    assert(one - two == 0);
    assert(x == 48);
}

int f2(int a, int b) {
    assert(a-b == 0);

    return 48;
}
