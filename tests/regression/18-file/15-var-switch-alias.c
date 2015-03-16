// PARAM: --set ana.activated[+] "'file'" --enable  ana.file.optimistic

#include <stdio.h>

int main(){
	FILE *fp1;
	fp1 = fopen("test.txt", "a");
	fprintf(fp1, "Testing...\n");

	FILE *fp2;
	fp2 = fopen("test.txt", "a"); // WARN: file is never closed
	fprintf(fp2, "Testing...\n");

	fp2 = fp1;

	fclose(fp2);
	fclose(fp1); // WARN: closeing already closed file handle fp1
} // WARN: unclosed files: fp2
