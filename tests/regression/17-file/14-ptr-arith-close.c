#include <stdio.h>

int main(){
	FILE *fp;
	fp = fopen("test.txt", "a");
	fprintf(fp, "Testing...\n");

	fp++; // WARN: changed file pointer fp (no longer safe)

	fclose(fp); // WARN: file may be never closed
}