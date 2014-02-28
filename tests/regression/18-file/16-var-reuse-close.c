#include <stdio.h>

int main(){
	FILE *fp;
	fp = fopen("test.txt", "a");
	fprintf(fp, "Testing...\n");
	fclose(fp);	

	fp = fopen("test.txt", "a"); // WARN: file is never closed
	fprintf(fp, "Testing...\n");
	// fclose(fp);
} // WARN: unclosed files: fp
