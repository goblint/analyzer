#include <stdio.h>

int main(){
	FILE *fp[3];
	// Array -> varinfo with index-offset
	fp[1] = fopen("test.txt", "a");
	fprintf(fp[1], "Testing...\n");
	fclose(fp[1]);


	struct foo {
		int i;
		FILE *fp;
	} bar;
	// Struct -> varinfo with field-offset
	bar.fp = fopen("test.txt", "a");
	fprintf(bar.fp, "Testing...\n");
	fclose(bar.fp);


	// Pointer -> Mem exp
	*(fp+2) = fopen("test.txt", "a");
	fprintf(*(fp+2), "Testing...\n");
	fclose(*(fp+2));
}

// All ok!