// PARAM: --set ana.activated[+] "'file'" --enable  ana.file.optimistic

#include <stdio.h>

FILE *fp;

int main(){
	int b;
	fp = fopen("test.txt", "a"); // WARN: MAYBE file is never closed

	if (b)
		fclose(fp);

	fprintf(fp, "Testing...\n"); // WARN: MAYBE writing to closed file handle fp

	if (!b)
		fclose(fp); // WARN: MAYBE closeing already closed file handle fp
} // WARN: MAYBE unclosed files: fp
