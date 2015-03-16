// PARAM: --set ana.activated[+] "'file'" --enable  ana.file.optimistic

#include <stdio.h>

FILE *fp;

int main(){
	int i;
	fp = fopen("test.txt", "a");  // WARN: MAYBE file is never closed

	while (i){ // May closed (11, 3), open(test.txt, Write) (7, 3)
		fprintf(fp, "Testing...\n"); // WARN: MAYBE writing to closed file handle fp
		fclose(fp); // WARN: MAYBE closeing already closed file handle fp
		i++;
	}
	// why: fp -> Must open(test.txt, Write) (7, 3)
	// -> because loop wouldn't exit?
} // WARN: MAYBE unclosed files: fp
