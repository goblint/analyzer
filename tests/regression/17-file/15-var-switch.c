#include <stdio.h>

int main(){
	FILE *fp1;
	fp1 = fopen("test.txt", "a");
	fprintf(fp1, "Testing...\n");

	FILE *fp2;	// Warn here: fp2 not closed
	fp2 = fopen("test.txt", "a");
	fprintf(fp2, "Testing...\n");

	fp2 = fp1;

	fclose(fp1);
	fclose(fp2); // Warn here: fp1 already closed
}