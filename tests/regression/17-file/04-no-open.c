#include <stdio.h>

FILE *fp;

int main(){
	fprintf(fp, "Testing...\n"); // Warn here: fp not open?
	fclose(fp);
}