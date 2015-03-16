// PARAM: --set ana.activated[+] "'file'" --enable  ana.file.optimistic

#include <stdio.h>

int main(){
	FILE *fp1;
	fp1 = fopen("test.txt", "a");
	FILE **fp2;

	fp2 = &fp1;

	fclose(fp1);
	fclose(*fp2); // WARN: closeing already closed file handle fp1
}
