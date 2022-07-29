//PARAM: --enable ana.int.interval --disable ana.int.def_exc --set ana.base.arrays.domain partitioned
#include <assert.h>

void foo(int (*a)[40]){
    int x = (*(a + 29))[7];
    __goblint_check(x == 23); //FAIL

    int y = (*(a + 7))[13];
    __goblint_check(y == 23);

    __goblint_check(a[7][13] == 23);
}

void foo2(int n,int (*a)[n]){
    int x = (*(a + 29))[7];
    __goblint_check(x == 23); //FAIL

    int y = (*(a + 7))[13];
    __goblint_check(y == 23);

    __goblint_check(a[7][13] == 23);
}

void foo3(int n,int a[][n]){
    int x = (*(a + 29))[7];
    __goblint_check(x == 23); //FAIL

    int y = (*(a + 7))[13];
    __goblint_check(y == 23);

    __goblint_check(a[7][13] == 23);
}

void foo4(int n,int a[n][n]){
    int x = (*(a + 29))[7];
    __goblint_check(x == 23); //FAIL

    int y = (*(a + 7))[13];
    __goblint_check(y == 23);

    __goblint_check(a[7][13] == 23);
}

void foo5(int n, int m, int a[n][m]){
    int x = (*(a + 29))[7];
    __goblint_check(x == 23); //FAIL

    int y = (*(a + 7))[13];
    __goblint_check(y == 23);

    __goblint_check(a[7][13] == 23);
}

int main(void)
{
    int n =40;
    int b[n][n];

    for(int i=0;i < 40; i++) {
        for(int j=0; j<40;j++) {
            b[i][j] = 0;
        }
    }

    b[7][13] = 23;

    foo(b);
    foo2(40,b);
    foo3(40,b);
    foo4(40,b);
    foo5(40,40,b);
}
