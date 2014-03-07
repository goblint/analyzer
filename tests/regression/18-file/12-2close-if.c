// PARAM: --set ana.activated[0][+] "'file'" --enable  ana.file.optimistic

#include <stdio.h>

int main(){
	FILE *fp;
	int b;
	fp = fopen("test.txt", "a");
	fprintf(fp, "Testing...\n");

	if (b)
		fclose(fp);

	fclose(fp); // WARN: MAYBE closeing already closed file handle fp
}
