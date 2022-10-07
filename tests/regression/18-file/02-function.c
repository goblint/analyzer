// PARAM: --set ana.activated[+] "'file'" --enable  ana.file.optimistic --disable warn.info

#include <stdio.h>

FILE *fp;

void f(){
	fp = fopen("test.txt", "a"); // NOWARN
}

int main(){
	f();
	fprintf(fp, "Testing...\n"); // NOWARN
	fclose(fp); // NOWARN
} // NOWARN

// All ok!
