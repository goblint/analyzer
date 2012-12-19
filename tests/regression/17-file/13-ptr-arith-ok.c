#include <stdio.h>

int main(){
	FILE *fp;
	fp = fopen("test.txt", "a");
	fprintf(fp, "Testing...\n");

	fp++;
	fp--;

	fclose(fp); 
}

// All ok!