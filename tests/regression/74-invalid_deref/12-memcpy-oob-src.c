// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --disable warn.info
// TODO: The "--disable warn.info" part is a temporary fix and needs to be removed once the MacOS CI job is fixed
#include <stdlib.h>
#include <string.h>

struct A {
	unsigned char a;
	unsigned char b:2;
	unsigned char c:2;
	unsigned char d:5;
	unsigned char e;
} __attribute__((packed));

struct A d;
int main(void)
{
	struct A *p;
	p = malloc(5);
	d.a = 1;
	d.b = 2;
	d.c = 3;
	d.d = 4;
	d.e = 5;
    // It's an OOB error, because sizeof(d) == 4
	memcpy(p, &d, 5); //WARN
	if (p->a != 1) {
		free(p);
	}
	if (p->b != 2) {
		free(p);
	}
	if (p->c != 3) {
		free(p);
	}
	if (p->d != 4) {
		free(p);
	}
	if (p->e != 5) {
		free(p);
	}
	free(p);
}

