// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs

struct a_str{
    int x;
    double y;
    int* ptr;
};

struct b_str{
    double x;
    int y;
    double * ptr;
};


int foo(void *p, void *p2){
    struct a_str* p_a = (struct a_str*) p;
    struct b_str* p2_b = (struct b_str*) p2;

    int* p3 = p_a->ptr;

    int top;
    if(top){
        *p3 = 23;
    }
    return 0;
}

int main(){
    int x = 32;
    double y = 42.1;
    struct a_str a = {0, 0.0, &x};
    struct b_str b = {12, 0.1, &y};
    assert(x == 32);
    foo(&a, &b);
    assert(x == 32); //UNKNOWN!
    return 0;
}
