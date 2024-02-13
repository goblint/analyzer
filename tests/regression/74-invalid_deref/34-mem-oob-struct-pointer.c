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
    p = malloc(9);

    struct A f = *p;       // NOWARN
    struct A f = *(p + 1); // WARN
    struct A f = *(p + 2); // WARN

    free(p);
}
