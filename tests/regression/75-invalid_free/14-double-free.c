//PARAM: --set ana.activated[+] useAfterFree
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <wchar.h>

struct A19 {
 int *p;
};

void init(struct A19 *a) {
 a->p = (int *)malloc(sizeof(int));

 free(a->p);
 free(a->p); //WARN
}


void main(void) {
 struct A19 a19;
 a19.p = NULL;
 init(&a19);
}