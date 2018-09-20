#include <stdio.h>
#include <stdlib.h>

int main() {

	char *p1;
	p1[10] = 'A'; // WARN
	p1 = malloc(100);

	char *p2 = malloc(100);
	char *p3 = malloc(100);
	char p4[10];

	p4[100] = 'A'; // WARN

	memset(p1, 0, 100);

	memcpy(p3, p2, 100); // WARN
	memcpy(p2, p1, 100);
	memcpy(p3, p2, 100);

	p1[10] = 'A';
	int j = 100;
	p1[j] = 'A'; // WARN
	p1[100] = 'A'; // WARN
	*(p1+100) = 'A'; // WARN

	printf("%s%d", "aaa");

	free(p1);
	free(p2);
	free(p3);

}

