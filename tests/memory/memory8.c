#include <stdio.h>
#include <stdlib.h>

int main() {

	char *p1 = malloc(100);

	int i = 0;

	for(i=0; i<10; i++)
	{
		p1[i] = i+60;
	}
	p1[10] = 0;

	puts(p1);
	free(p1);
}

