// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval
#include <stdlib.h>

struct A {
	unsigned char a;
	unsigned char b:2;
	unsigned char c:2;
	unsigned char d;
} __attribute__((packed));

int main(void)
{
	struct A *p;
	p = malloc(2);
	p->a = 1;
	if (p->a != 1) {
		free(p);
	}
	p->b = 2;
	if (p->b != 2) {
		free(p);
	}
	p->c = 3;
	if (p->c != 3) {
		free(p);
	}
	p->d = 4; //WARN
	if (p->d != 4) {//WARN
		free(p);
	}
	free(p);
}

