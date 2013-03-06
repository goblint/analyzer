#include <stdio.h>

int main(){
	FILE *fp;
	fp = fopen("test.txt", "a"); // WARN: file may be never closed
	fprintf(fp, "Testing...\n");

	fp++; // WARN: changed file pointer fp (no longer safe)
	fp--; // WARN: changed file pointer fp (no longer safe)

	fclose(fp); 
} // WARN: maybe unclosed files: fp

// OPT: All ok!