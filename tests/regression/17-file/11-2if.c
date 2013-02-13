#include <stdio.h>

FILE *fp;

int main(){
	int b;
	fp = fopen("test.txt", "a"); // WARN: file may be never closed

	if (b)
		fclose(fp);

	fprintf(fp, "Testing...\n"); // WARN: might be writing to closed file handle fp

	if (!b)
		fclose(fp); // WARN: might be closeing already closed file handle fp
} // WARN: maybe unclosed files: fp
