#include <stdio.h>

int main(){
	FILE *fp;
	fp = fopen("test.txt", "a"); // WARN: MAYBE file is never closed
	fprintf(fp, "Testing...\n");
	int b;
	if(b)
		return 1; // WARN: MAYBE unclosed files: fp
	fclose(fp);
	return 0;
}
