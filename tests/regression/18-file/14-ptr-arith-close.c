#include <stdio.h>

int main(){
	FILE *fp;
	fp = fopen("test.txt", "a"); // WARN: MAYBE file is never closed
	fprintf(fp, "Testing...\n");

	fp++; // WARN: changed file pointer fp (no longer safe)

	fclose(fp); // might be closeing anything
} // WARN: MAYBE unclosed files: fp