#include <stdio.h>
#include <stdlib.h>

int main() {

	char *p1 = malloc(100);
	char *p2;

	gets_s(p1, 4);
	gets_s(p1, 400);

	fread(p1, 100, 2, NULL);
	fread(p1, 3, 3, NULL);
	
	free(p1);
	

}

