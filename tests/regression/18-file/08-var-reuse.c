// PARAM: --set ana.activated[+] "'file'" --enable  ana.file.optimistic --disable warn.info

#include <stdio.h>

int main(){
	FILE *fp;
	fp = fopen("test.txt", "a"); // NOWARN
	fprintf(fp, "Testing...\n"); // NOWARN
	fclose(fp); // NOWARN
	fp = fopen("test2.txt", "a"); // NOWARN
	fprintf(fp, "Testing...\n"); // NOWARN
	fclose(fp); // NOWARN
} // NOWARN

// All ok!
