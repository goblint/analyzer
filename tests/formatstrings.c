#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>

int main() {

	char buf1[40];
	char *buf2 = malloc(60);
	char *buf3 = malloc(rand());
	
	int i = 1;
	unsigned int ui = 200;
	float d = 2.2;
	ptrdiff_t p = 0;
	// NO WARN
	printf("%10.f -> %td\n", d, p);
	printf("%100.4d%n", i, &i);
	printf("%x%x\n", ui, ui+1);
	printf("%x\n", 10);
	printf("%f\n", 3.14);
	printf("%.10s", "Hallo");
	scanf("%f%10s", &d, buf1);
	scanf("%*s%10s%*d", buf1);

	// No. of args
	printf(" ->%d-%d-%d<- \n", i, i); // WARN NOT ENOUGH ARGS
	printf("%d%.10s", i); // WARN NOT ENOUGH ARGS
	printf("%d", i, i); // WARN TOO MANY ARGS

	// Argtypes
	printf("%.80s -> %d", buf1, buf1); // WARN WRONG TYPE FOR INDEX 1
	printf("%.*s", d, buf1); // WARN WRONG TYPE FOR INDEX 0
	printf("%x", -100); // WARN ILLEGAL TYPE FOR INDEX 0
	scanf("%td%d", &p, i); // WARN WRONG TYPE FOR INDEX 2
	scanf("%10s", "const char"); // WARN ILLEGAL TYPE FOR INDEX 0

	// Length Modifiers
	printf("%.10zs", buf1); // WARN UNDEFINED LENGTH MODIFIER
	printf("%Ld", i); // WARN UNDEFINED LENGTH MODIFIER
	printf("%hhp", p); // WARN UNDEFINED LENGTH MODIFIER 

	// Flags
	printf("%#d", i); // WARN INCORRECT FLAG 
	printf("%0s", buf1); // WARN INCORRECT FLAG 

	// Bufsizes
	printf("-->%s<--", buf1); // WARN UNDELIMITED STRING
	scanf("-->%s<--", buf1); // WARN UNDELIMITED STRING
	scanf("%100s", buf1); // WARN BUF1 TOO SMALL
	scanf("%100c", buf1); // WARN BUF1 TOO SMALL
	scanf("%100[abc]", buf1); // WARN BUF1 TOO SMALL
	scanf("%40s -------- %40c", buf2, buf1); // WARN BUF1 TOO SMALL (OFF BY 1)
	scanf("%40[abc]-%100[^abc]", buf1, buf2); // WARN BUF1 TOO SMALL (OFF BY 1) AND BUF2 TOO SMALL

	// May Warnings
	scanf("%20s", buf3);
	printf(buf3);

}

