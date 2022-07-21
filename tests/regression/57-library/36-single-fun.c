// NOMARSHAL PARAM: --enable ana.library.enabled  --sets ana.activated[-] threadid  --sets ana.activated[+] mallocWrapperTypeBased  --sets ana.activated[+] typecasts --sets ana.activated[-] mallocWrapper --sets ana.activated[+] writtenLvals --sets ana.activated[+] varArgs --disable ana.library.all --set mainfun "['bar']" --sets ana.activated[-] base


// This
#include<stdlib.h>

typedef struct s {int a; long *b;} my_struct;

void bar(my_struct *y){ //DEADCODE
    long x = 3;
    y->b = &x;

    long *p = NULL;
    p = y->b;
    y->a = 4;
    *p = 5;

    assert(x == 3); // UNKNOWN
}

void foo(my_struct *y){ //DEADCODE
    long *p = y->b;
    *p = 0;
}

int main32(){
    my_struct *p = malloc(sizeof(my_struct));
    p->a = 2;
    bar(p);
    return 2;
}
