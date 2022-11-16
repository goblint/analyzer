// SKIP PARAM: --enable ana.int.interval --set ana.base.arrays.domain partitioned --disable ana.base.context.int --set ana.activated[+] taintPartialContexts
#include <goblint.h>

void g(int *x){
    return;
}


void f(int a[20], int *jptr) {
    while (*jptr < 10) {
        a[*jptr] = 1;
        *jptr = *jptr + 1;
    }

}

int main() {
    int arry[20];
    int other;
    int j = 0;
    int i = 0;

    //init arry
    while (i < 20) {
        arry[i] = 0; 
        i++;
    }

    //partition arry with j
    while (j < 5) {
        arry[j] = 1;
        j++;
    }

    __goblint_check(arry[0] == 1);  //SUCCESS
    __goblint_check(arry[5] == 0);  //SUCCESS

    //call f with arry and pointer to j

    //int *jptr = &j;
    f(arry, &j);

    __goblint_check(arry[0] == 1);  //SUCCESS
    __goblint_check(arry[5] == 0);  //FAIL
    __goblint_check(arry[10] == 0);  //SUCCESS

     




}