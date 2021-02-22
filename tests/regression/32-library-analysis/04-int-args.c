//PARAM: --enable allfuns --enable ana.library  --set ana.activated "['base','mallocWrapperTypeBased']"

int f(int *x, int *y){
    int z = 1;
    if(x == y){
        z = 0;
    }
    assert(z == 1); //UNKNOWN!
}
