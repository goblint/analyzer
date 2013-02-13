#include <stdio.h>

FILE *fp;

int main(){
	int b;
	fp = fopen("test.txt", "a");  // WARN: file may be never closed

	fprintf(fp, "Testing...\n");

	if (b)
		fclose(fp);
} // WARN: maybe unclosed files: fp
