//PARAM: --set ana.ctx_insens[+] base --set ana.activated[+] taintPartialContexts --set ana.base.arrays.domain unroll --set ana.base.arrays.unrolling-factor 2
#include <goblint.h>

struct myStruct {
    int n;
    int q;
};

struct myStruct globStrct;
    
int globArry[2];
int globInt;

void f(int* mem) {
    globArry[1] = 70;
    globStrct.q = 70;
    *(mem + 1) = 70;
}

int main () {
    int *locMem = malloc(sizeof(int)*2);

    // init
    globInt = 1;
    globStrct.n = 1;
    globStrct.q = 1;
    globArry[0] = 1;
    globArry[1] = 1;
    *(locMem) = 1;
    *(locMem + 1) = 1;
    
    f(locMem);

    //change
    globInt = 2;
    globStrct.n = 2;
    globStrct.q = 2;
    globArry[0] = 2;
    globArry[1] = 2;
    *(locMem) = 2;
    *(locMem + 1) = 2;

    f(locMem);

    //check untainted
    __goblint_check(globInt == 2);
    __goblint_check(globStrct.n == 2);
    __goblint_check(globArry[0] == 2);
    __goblint_check(*(locMem) == 2); //UNKNOWN

    //validate tainted
    __goblint_check(globStrct.q == 70);
    __goblint_check(globArry[1] == 70);
    __goblint_check(*(locMem) == 70); //UNKNOWN

    free(locMem);
}
