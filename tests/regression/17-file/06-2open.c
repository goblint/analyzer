#include <stdio.h>

FILE *fp;

int main(){
	fp = fopen("test1.txt", "a"); // WARN: file is never closed
	fp = fopen("test2.txt", "a"); // WARN: overwriting still opened file handle fp
	fprintf(fp, "Testing...\n"); 
	fclose(fp);
}