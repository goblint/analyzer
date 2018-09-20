#include <stdio.h>
#include <stdlib.h>

int main() {

	void *p1 = malloc(100);
	void *p2 = malloc(200);

	memset(p1,0,100);

	memcpy(p2,p1,200); 

	memset(p2,0,200);

	memcpy(p1,p2,200);
	
	free(p1);
	free(p2);
	

}

