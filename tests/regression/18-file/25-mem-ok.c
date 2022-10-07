// PARAM: --set ana.activated[+] "'file'" --enable  ana.file.optimistic --disable warn.info

#include <stdio.h>

int main(){
	FILE *fp[3];
	// Array -> varinfo with index-offset
	fp[1] = fopen("test.txt", "a"); // NOWARN
	fprintf(fp[1], "Testing...\n"); // NOWARN
	fclose(fp[1]); // NOWARN


	struct foo {
		int i;
		FILE *fp;
	} bar;
	// Struct -> varinfo with field-offset
	bar.fp = fopen("test.txt", "a"); // NOWARN
	fprintf(bar.fp, "Testing...\n"); // NOWARN
	fclose(bar.fp); // NOWARN


	// Pointer -> Mem exp
	*(fp+2) = fopen("test.txt", "a"); // NOWARN
	fprintf(*(fp+2), "Testing...\n"); // NOWARN
	fclose(*(fp+2)); // NOWARN
} // NOWARN

// All ok!
