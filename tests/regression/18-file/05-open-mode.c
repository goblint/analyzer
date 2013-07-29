#include <stdio.h>

FILE *fp;

int main(){
	fp = fopen("test.txt", "r"); 
	fprintf(fp, "Testing...\n"); // WARN: writing to read-only file handle fp
	fclose(fp);
}