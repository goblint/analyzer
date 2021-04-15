//PARAM: --enable ana.int.interval
#include <stdio.h>

int f(int x, int y){
    int sum = x + y;
    return sum * sum;
}

int main(){
    int x = 0;
    int y = 4;
    int *py = &y;
    int fac = 1;

    while(y != 0){
        fac = fac * y;
        *py = *py - 1;
    }

    printf("fac 4: %d\n", fac);

    int res = f(3, 5);

    assert(res == 64);
    printf("result of f: %d\n", res);

    return res;
}
