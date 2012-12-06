#include <stdio.h>

int main(){
	// working example
	FILE *fp;
	fp = fopen("test.txt", "a");
	fprintf(fp, "Testing...\n");
	fclose(fp);

	return 0;
}
