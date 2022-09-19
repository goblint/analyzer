//PARAM: --enable ana.int.enums --disable ana.int.def_exc
#include <assert.h>

int main(){
    int top = rand();
    int top2 = rand();
    int x,y;

    if(top){
        x = 1;
    } else{
        x = 0;
    }

    if(top2){
        y = 1;
    } else{
        y = 0;
    }

    __goblint_check(x < 2);
    __goblint_check(x < 1); // UNKNOWN!
    __goblint_check(x < 0); // FAIL

    __goblint_check(x <= 2);
    __goblint_check(x <= 1);
    __goblint_check(x <= 0); // UNKNOWN!
    __goblint_check(x <= -1); //FAIL

    __goblint_check(x > -1);
    __goblint_check(x > 0); //UNKNOWN!
    __goblint_check(x > 1); //FAIL

    __goblint_check(x >= -1);
    __goblint_check(x >= 0);
    __goblint_check(x >= 1); //UNKNOWN!
    __goblint_check(x >= 2); //FAIL

    __goblint_check(x == y); // UNKNOWN
    __goblint_check(x == 1); // UNKNOWN
    __goblint_check(x == 2); // FAIL

    __goblint_check(x != y); // UNKNOWN
    __goblint_check(x != 1); // UNKNOWN
    __goblint_check(x != 2);

    int z = rand();
    y = 3;
    if(z==3){
        __goblint_check(y==z);
        __goblint_check(y!=z); //FAIL
    } else {
        __goblint_check(y==z); //FAIL
        __goblint_check(y!=z);
    }

    return 0;
}
