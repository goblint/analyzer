// PARAM: --set ana.activated[+] wrpointer
// example of the paper "2-Pointer Logic" by Seidl et al., Example 9, pag. 22
#include <stdlib.h>
#include <goblint.h>

void main(void) {
    int *x = (int*)malloc(sizeof(int));
    int **z = (int**)malloc(sizeof(int*));
    *z = x;
    int *y = (int*)malloc(sizeof(int*));
    int top;
    if(top){
        y = (int*)z;
        *x = (long)z;
    }
    *y = -1 + *x;

    __goblint_check(*z == x);
    __goblint_check(*y == -1 + *x);

    **z = 1 + *x;

    __goblint_check(x == *z);
    __goblint_check(*y == -2 + *x);

}
