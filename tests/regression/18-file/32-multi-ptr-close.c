// PARAM: --set ana.activated[+] "'file'" --enable  ana.file.optimistic

#include <stdio.h>

int main(){
	FILE *fp1;
	fp1 = fopen("test1.txt", "a");
	fprintf(fp1, "Testing...\n");

	FILE *fp2;
	fp2 = fopen("test2.txt", "a");
	fprintf(fp2, "Testing...\n");

	FILE **fp;
	int b;
	if(b){
		fp = &fp1;
	}else{
		fp = &fp2;
	}

	fclose(*fp);
	fclose(fp1); // WARN: MAYBE closeing already closed file handle fp1
	fclose(fp2); // WARN: MAYBE closeing already closed file handle fp2
}
