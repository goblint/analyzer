#include <stdio.h>

int main(){

	// working example
	FILE *fp;
	fp = fopen("test.txt", "a");
	fprintf(fp, "Testing...\n");
	fclose(fp);

	// missing fopen -> compiles, but leads to Segmentation fault
	FILE *fp2;
	// fp2 = fopen("test.txt", "a");
	fprintf(fp2, "Testing...\n");
	fclose(fp2);

	// writing to a read-only file -> doesn't do anything
	FILE *fp3;
	fp3 = fopen("test.txt", "r");
	fprintf(fp3, "Testing...\n");
	fclose(fp3);

	// accessing closed file -> write doesn't do anything
	FILE *fp4;
	fp4 = fopen("test.txt", "a");
	fclose(fp4);
	fprintf(fp4, "Testing...\n");

	// bad style:
	// opening file but not doing anything

	// missing fclose

	return 0;
}