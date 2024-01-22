// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval
#include <stdlib.h>

struct A {
unsigned int a;
unsigned int b;
};

int main(void)
{
    struct A *p;
    struct A *f;
    p = malloc(9);
    
    p->a = 1; // NOWARN

    f = p + 1;

    f->b = 2; // WARN
    (f-1)->a = 3;
    (f-1+(2))->b = 4;
    free(p);
}

