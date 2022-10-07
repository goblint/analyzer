// PARAM: --set ana.activated[+] "'file'" --enable  ana.file.optimistic --disable warn.info

#include <stdio.h>

int main(){
	FILE *fp;
	FILE* (*f)(const char *, const char*);
	f = fopen;
	fp = f("test.txt", "a"); // NOWARN
	fprintf(fp, "Testing...\n"); // NOWARN
	fclose(fp); // NOWARN
} // NOWARN

// All ok!
