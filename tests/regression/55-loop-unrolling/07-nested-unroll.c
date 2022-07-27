// PARAM: --set solver td3 --enable ana.int.interval --set exp.unrolling-factor 5 --set ana.base.arrays.domain unroll --set ana.base.arrays.unrolling-factor 5
#include<assert.h>
int main(void) {
    int arr[10][10];

    for(int i=0;i<10; i++) {
        for(int j=0;j <10; j++) {
            arr[i][j] = i+j;
        }
    }

    for(int i=0;i<5; i++) {
        for(int j=0;j <5; j++) {
            __goblint_check(arr[i][j] == i+j);
        }
    }
}
