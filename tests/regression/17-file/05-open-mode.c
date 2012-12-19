#include <stdio.h>

FILE *fp;

int main(){
	fp = fopen("test.txt", "r"); 
	fprintf(fp, "Testing...\n"); // Warn here: fp not open for writing?
	fclose(fp);
}