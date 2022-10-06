// PARAM: --set ana.activated[+] "'file'" --enable  ana.file.optimistic --disable warn.info

#include <stdio.h>

FILE *fp;

int main(){
	int i;
	fp = fopen("test.txt", "a"); // NOWARN

	while (i){
		fprintf(fp, "Testing...\n"); // NOWARN
		i++;
	}

	fclose(fp); // NOWARN
} // NOWARN

// All ok.
