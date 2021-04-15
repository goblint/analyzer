//PARAM: --enable ana.int.interval
#include <stdio.h>
#include <assert.h>

int f(int x, int y){
    int sum = x + y;
    return sum * sum;
}

int main(){
    int x = 0;
    int y = 100;
    int *py = &y;
    int fac = 1;

    while(y != 0){
        fac = fac * y;
        *py = *py - 1;
    }
    assert(y == 0);

    printf("fac 100: %d\n", fac);

    int res = f(3, 5);

    assert(res == 64);
    printf("result of f: %d\n", res);

    return res;
}
