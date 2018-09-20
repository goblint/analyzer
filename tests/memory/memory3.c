#include <stdio.h>
#include <stdlib.h>

int main() {

	void *p1 = calloc(100, 100);
	void *p2 = calloc(100, 100);
	void *p3;

	int i = 0;
	scanf("%d", i);

	if(i == 1)
		p3 = p1;
	else
		p3 = p2;

	strcpy(p3,p2);

	free(p3);
	free(p1);
	

}

