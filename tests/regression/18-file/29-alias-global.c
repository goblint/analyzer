// PARAM: --set ana.activated[+] "'file'" --enable  ana.file.optimistic

#include <stdio.h>

FILE* fp;
FILE* myfopen(char* f){
	fp = fopen(f, "a");
	return fp;
}

int main(){
	FILE *fp1;
	FILE *fp2;
	fp1 = myfopen("test1.txt");
	fp2 = myfopen("test2.txt");
	fprintf(fp1, "Testing...\n");
	fclose(fp1);
	fprintf(fp2, "Testing...\n");
	fclose(fp2);
}

// All ok!
