// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs


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
