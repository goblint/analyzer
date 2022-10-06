// PARAM: --set ana.activated[+] "'file'" --enable  ana.file.optimistic --disable warn.info

#include <stdio.h>

FILE* fp;
FILE* myfopen(char* f){
	fp = fopen(f, "a"); // NOWARN
	return fp;
}

int main(){
	FILE *fp1;
	FILE *fp2;
	fp1 = myfopen("test1.txt");
	fp2 = myfopen("test2.txt");
	fprintf(fp1, "Testing...\n"); // NOWARN
	fclose(fp1); // NOWARN
	fprintf(fp2, "Testing...\n"); // NOWARN
	fclose(fp2); // NOWARN
} // NOWARN

// All ok!
