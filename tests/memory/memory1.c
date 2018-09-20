#include <stdio.h>
#include <stdlib.h>

int main() {

	void *p1 = malloc(100);
	void *p2 = malloc(100);

	int i = 100;

	memset(p1, 0, i);

	strcpy(p1, p1); // WARN
	memcpy(p2, p1, 100); // WARN

	free(p1);
	
	realloc(p2,0);

}

