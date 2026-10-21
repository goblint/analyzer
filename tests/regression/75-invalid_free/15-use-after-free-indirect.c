//PARAM: --set ana.activated[+] useAfterFree
#include <stdlib.h>

struct A19 {
 	int *p;
};


void init_and_free(struct A19 *a) {
	(*a).p = (int *)malloc(sizeof(int));

	free(a);
	// a is freed; use-after-free
	*((*a).p) = 3; //WARN

	// a already freed; Thus, this not a double free, but a use-after-free.
	free((*a).p); //WARN
}

int main(void) {
	struct A19 *a19p = (struct A19 *)malloc(sizeof(struct A19));
	init_and_free(a19p);
	return 0;
}
