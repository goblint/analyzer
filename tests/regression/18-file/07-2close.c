// PARAM: --set ana.activated[0][+] "'file'" --enable  ana.file.optimistic

#include <stdio.h>

int main(){
	FILE *fp;
	fp = fopen("test.txt", "a");
	fprintf(fp, "Testing...\n");
	fclose(fp);
	fclose(fp); // WARN: closeing already closed file handle fp
}
