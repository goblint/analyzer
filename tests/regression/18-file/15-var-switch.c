#include <stdio.h>

int main(){
	FILE *fp1;
	fp1 = fopen("test.txt", "a");
	fprintf(fp1, "Testing...\n");

	FILE *fp2;
	fp2 = fopen("test.txt", "a"); // WARN: file is never closed
	fprintf(fp2, "Testing...\n");

	fp2 = fp1;

	fclose(fp1);
	fclose(fp2); // WARN: closeing already closed file handle fp2
} // WARN: unclosed files: fp2