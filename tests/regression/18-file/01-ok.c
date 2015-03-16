// PARAM: --set ana.activated[+] "'file'" --enable  ana.file.optimistic

#include <stdio.h>

int main(){
	FILE *fp;
	fp = fopen("test.txt", "a");
	fprintf(fp, "Testing...\n");
	fclose(fp);
}

// All ok!
