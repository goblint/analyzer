#include <stdio.h>

FILE *fp;

int main(){
	int i;
	fp = fopen("test.txt", "a");  // WARN: file may be never closed

	while (i){ // May closed (11, 3), open(test.txt, Write) (7, 3) 
		fprintf(fp, "Testing...\n"); // WARN: might be writing to closed file handle fp
		fclose(fp); // WARN: might be closeing already closed file handle fp
		i++;
	}
	// why: fp -> Must open(test.txt, Write) (7, 3)
	// -> because loop wouldn't exit?
} // WARN: maybe unclosed files: fp
