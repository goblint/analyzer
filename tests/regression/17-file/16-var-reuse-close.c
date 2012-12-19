#include <stdio.h>

int main(){
	FILE *fp;
	fp = fopen("test.txt", "a");
	fprintf(fp, "Testing...\n");
	fclose(fp);	

	fp = fopen("test.txt", "a"); // Warn here: fp not closed
	fprintf(fp, "Testing...\n");
	// fclose(fp);
}
