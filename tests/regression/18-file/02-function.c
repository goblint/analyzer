// PARAM: --set ana.activated[+] "'file'" --enable  ana.file.optimistic

#include <stdio.h>

FILE *fp;

void f(){
	fp = fopen("test.txt", "a");
}

int main(){
	f();
	fprintf(fp, "Testing...\n");
	fclose(fp);
}

// All ok!
