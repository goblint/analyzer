//PARAM: --enable ana.int.enums --disable ana.int.def_exc
int main(){
    int top = rand();
    int x,y;

    if(top){
        x = 1;
    } else{
        x = 0;
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
    return 0;
}
