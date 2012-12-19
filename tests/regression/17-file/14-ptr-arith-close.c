#include <stdio.h>

int main(){
	FILE *fp;
	fp = fopen("test.txt", "a");
	fprintf(fp, "Testing...\n");

	fp++;

	fclose(fp); // Warn here: fp not closed 
}