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

    assert(x < 2);
    assert(x < 1); // UNKNOWN!
    assert(x < 0); // FAIL

    assert(x <= 2);
    assert(x <= 1);
    assert(x <= 0); // UNKNOWN!
    assert(x <= -1); //FAIL

    assert(x > -1);
    assert(x > 0); //UNKNOWN!
    assert(x > 1); //FAIL

    assert(x >= -1);
    assert(x >= 0);
    assert(x >= 1); //UNKNOWN!
    assert(x >= 2); //FAIL

    assert(x == y); // UNKNOWN
    assert(x == 1); // UNKNOWN
    assert(x == 2); // FAIL

    assert(x != y); // UNKNOWN
    assert(x != 1); // UNKNOWN
    assert(x != 2);

    int z = rand();
    y = 3;
    if(z==3){
        assert(y==z);
        assert(y!=z); //FAIL
    } else {
        assert(y==z); //FAIL
        assert(y!=z);
    }

    return 0;
}
