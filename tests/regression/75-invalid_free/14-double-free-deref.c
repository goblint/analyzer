//PARAM: --set ana.activated[+] useAfterFree
#include <stdlib.h>

struct A19 {
 	int *p;
};

void init_and_free(struct A19 *a) {
	a->p = (int *)malloc(sizeof(int));

	free(a->p);
	free(a->p); //WARN
}


int main(void) {
	struct A19 a19;
	a19.p = NULL;
	init_and_free(&a19);
	return 0;
}
