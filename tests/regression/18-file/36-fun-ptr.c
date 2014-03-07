// PARAM: --set ana.activated[0][+] "'file'" --enable  ana.file.optimistic

#include <stdio.h>

int main(){
	FILE *fp;
	FILE* (*f)(const char *, const char*);
	f = fopen;
	fp = f("test.txt", "a");
	fprintf(fp, "Testing...\n");
	fclose(fp);
}

// All ok!
