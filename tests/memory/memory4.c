#include <stdio.h>
#include <stdlib.h>

int main() {

	void *p1 = malloc(200);
	void *p2 = malloc(100);
	void *p3 = malloc(100);
	void *p4;

	memset(p1,0,200);

	memcpy(p2,p1,30);

	memcpy(p3,p2,100); // WARN

	memcpy(p2,p1,100);

	memcpy(p3,p2,100);

	free(p1);
	free(p2);
	free(p3);
	

}

