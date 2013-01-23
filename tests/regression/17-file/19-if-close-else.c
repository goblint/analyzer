#include <stdio.h>

FILE *fp;

int main(){
	int b;
	fp = fopen("test.txt", "a");  // Warn here: fp might not be closed?

	if (b)
		fclose(fp);
	else
		fprintf(fp, "Testing...\n");
	
	fclose(fp);
}
