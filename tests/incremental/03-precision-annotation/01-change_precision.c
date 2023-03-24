//PARAM: --enable annotation.int.enabled
#include <goblint.h>
#include <math.h>

int main() __attribute__ ((goblint_precision("def_exc","interval")));

int foo(int x){
    if(x < 10){
        return 0;
    }
    return 1;
}

int bar(int x){
    if(x < 10){
        return 0;
    }
    return 1;
}

int main(){
    int x = rand() % 10;

    int a = foo(x);
    __goblint_check(a == 0); //UNKNOWN

    int b = bar(x);
    __goblint_check(b == 0); //UNKNOWN
    return 0;
}
