//PARAM: --enable ana.int.enums --disable ana.int.def_exc
int main(){
    int top = rand();
    int x,y;

    if(top){
        x = 1;
    } else{
        x = 0;
    }

    assert(x<2);
    return 0;
}
