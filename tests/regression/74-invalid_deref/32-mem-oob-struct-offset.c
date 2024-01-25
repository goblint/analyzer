// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval
#include <stdlib.h>

struct A
{
    unsigned char a;
    unsigned char b;
    unsigned char c;
    unsigned char d;
    unsigned char e;
    unsigned char f;
    unsigned char g;
    unsigned char h;
};

int main(void)
{
    struct A *p;
    struct A *f;
    p = malloc(9);

    p->a = 1;  // NOWARN
    p->b = 2;  // NOWARN
    p->c = 3;  // NOWARN
    p->d = 4;  // NOWARN
    p->e = -1; // NOWARN
    p->f = -2; // NOWARN
    p->g = -3; // NOWARN
    p->h = -4; // NOWARN

    f = p + 1;

    f->a = 0; // NOWARN
    f->b = 0; // WARN
    f->c = 0; // WARN
    f->d = 0; // WARN
    f->e = 0; // WARN
    f->f = 0; // WARN
    f->g = 0; // WARN
    f->h = 0; // WARN

    struct A *t = p + 2;
    t->a = 0; // WARN

    free(p);
}
