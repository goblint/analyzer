#include <stdio.h>

FILE *fp;

int main(){
	fprintf(fp, "Testing...\n"); // WARN: writing to unopened file handle fp
	fclose(fp); // WARN: closeing unopened file handle fp
}