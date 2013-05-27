#include <stdio.h>

int main(){

	// no errors
	FILE *fp;
	fp = fopen("test.txt", "a");
	if(fp!=0) {
		fprintf(fp, "Testing...\n");
		fclose(fp);
	}

	// missing fopen -> compiles, but leads to Segmentation fault
	FILE *fp2;
	// fp2 = fopen("test.txt", "a");
	fprintf(fp2, "Testing...\n");	// WARN
	fclose(fp2);			// WARN

	// writing to a read-only file -> doesn't do anything
	FILE *fp3;
	fp3 = fopen("test.txt", "r");
	fprintf(fp3, "Testing...\n");	// (WARN)
	fclose(fp3);

	// accessing closed file -> write doesn't do anything
	FILE *fp4;
	fp4 = fopen("test.txt", "a");
	fclose(fp4);
	fprintf(fp4, "Testing...\n");	// WARN

	// missing fclose
	FILE *fp5;
	fp5 = fopen("test.txt", "a");	// WARN
	fprintf(fp5, "Testing...\n");

	// missing assignment to file handle
	fopen("test.txt", "a");		// WARN


	// bad style:
	// opening file but not doing anything

	return 0;			// WARN about all unclosed files
}