// NOMARSHAL PARAM: --enable ana.library.enabled --enable ana.library.all --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[-] base --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs

struct wrapper {
    int x;
    int *foo;
};

int g(void* val){
    struct wrapper* w = (struct wrapper*) val;

    return 0;
}


int foo(void* val){
    struct wrapper* w = (struct wrapper*) val;
    w->x = 12;
    return 0;
}


int foo_caller(){
    struct wrapper w;
    w.x = 12;
    w.foo = &w.x;
    foo(&w);
    return 0;
}

int main(char** args, int argc){
    int** ptr = (int**) args;
    return 0;
}


int fooBar(int* c){
    *c = 23;
    return 0;
}
