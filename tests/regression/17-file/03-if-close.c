#include <stdio.h>

FILE *fp;

int main(){
	int b;
	fp = fopen("test.txt", "a");  // WARN: MAYBE file is never closed

	fprintf(fp, "Testing...\n");

	if (b)
		fclose(fp);
} // WARN: MAYBE unclosed files: fp
