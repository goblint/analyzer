//PARAM: --enable ana.library --sets ana.activated[+] mallocWrapperTypeBased --sets ana.activated[-] mallocWrapper

int f(int *x, int *y){
    int z = 1;
    if(x == y){ // Pointers of same type might be equal or not
        z = 0;
    }
    assert(z == 1); //UNKNOWN!
    return 0;
}

struct data
{
    int *x;
    int *y;
};


int g(struct data d, int *y){
    int z = 1;
    if(d.x == y){
        z = 0;
    }
    assert(z == 1); //UNKNOWN!
    if(d.x == d.y){
        z = 0;
    }
    assert(z == 1); //UNKNOWN!
    return 0;
}


int h(int *x){
    int *y = malloc(sizeof(int));
    int z = 1;
    if(x == y){ // memory allocated within the function h should be detected different to memory reachable from arguments
        z = 0;
    }
    assert(z == 1);
    return 0;
}
