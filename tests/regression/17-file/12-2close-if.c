#include <stdio.h>

int main(){
	FILE *fp;
	fp = fopen("test.txt", "a");
	fprintf(fp, "Testing...\n");

	if (b)
		fclose(fp);

	fclose(fp); // Warn here: fp might already be closed
}
