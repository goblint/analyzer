// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval
#include <stdlib.h>

struct A {
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
    
    p->a = 1; // NOWARN

    f = p + 1;

    f->b = 2; // WARN

	free(p);
}

