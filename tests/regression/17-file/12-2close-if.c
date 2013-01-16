#include <stdio.h>

int main(){
	FILE *fp;
	int b;
	fp = fopen("test.txt", "a");
	fprintf(fp, "Testing...\n");

	if (b)
		fclose(fp);

	fclose(fp); // Warn here: fp might already be closed
}
