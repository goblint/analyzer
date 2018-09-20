#include <stdio.h>
#include <stdlib.h>

int main() {

	char *p1 = malloc(100);
	char *p2 = calloc(100, 1);
	char *p3;
	p3 = strncpy(p1, p2, 100);

	puts(p1);
	puts(p3);
	memmove(p1,p2,100);
	puts(p1);
	puts(p3);

	p1[10] = 'A';
	
	free(p1);
	free(p2);
}

