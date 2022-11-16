//PARAM: --disable ana.base.context.int --set ana.activated[+] taintPartialContexts
#include <goblint.h>

struct myStruct {
    int n;
    int q;
};

struct myStruct globStrct;
    
int globArry[3];

int globInt;


void f(int* mem, int* uninMem) {

    uninMem = malloc(16);

    globArry[0] = 70;
    globStrct.q = 70;

    globArry[6] = 70;


    *(mem + 3) = 70;
    *(mem + 2) = 70;

}

void f2() {


    globArry[0] = 70;
    globStrct.q = 70;

    globArry[6] = 70;


}


int main () {

    int *locMem = malloc(64);
    int *locMem2 = malloc(32);
    int *locUninMem;
    int *locUninMem2;

    globStrct.n = 1;
    globStrct.q = 1;

    globArry[0] = 1;
    globArry[1] = 1;
    globArry[2] = 1;

    globInt = 1;

    //f(locMem, locUninMem);
    f2();

    globStrct.n = 2;
    globStrct.q = 2;

    globArry[0] = 2;
    globArry[1] = 2;
    globArry[2] = 2;

    globInt = 2;


    //f(locMem, locUninMem2);
    f2();

    __goblint_check(globInt == 2); //SUCCESS
    __goblint_check(globArry[0] == 2); //UNKNOWN
    __goblint_check(globStrct.n == 2); //SUCCESS

    free(locMem);
    free(locMem2);
    //free(locUninMem);
    //free(locUninMem2);


}