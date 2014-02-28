#include <stdio.h>

int main(){
	FILE *fp;
	fp = fopen("test.txt", "a");
	fprintf(fp, "Testing...\n");
	fclose(fp);
	fp = fopen("test2.txt", "a");
	fprintf(fp, "Testing...\n");
	fclose(fp);
}

// All ok!