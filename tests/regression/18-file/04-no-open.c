// PARAM: --set ana.activated[0][+] "'file'" --enable  ana.file.optimistic

#include <stdio.h>

FILE *fp;

int main(){
	fprintf(fp, "Testing...\n"); // WARN: writing to unopened file handle fp
	fclose(fp); // WARN: closeing unopened file handle fp
}
