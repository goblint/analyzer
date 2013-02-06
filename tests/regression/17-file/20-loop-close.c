#include <stdio.h>

FILE *fp;

int main(){
	int i;
	fp = fopen("test.txt", "a");  // Warn here: fp might not be closed?

	while (i){ // May closed (11, 3), open(test.txt, Write) (7, 3) 
		fprintf(fp, "Testing...\n"); // why: fp -> Must open(test.txt, Write) (7, 3)
		fclose(fp);
		i++;
	}
	// why: fp -> Must open(test.txt, Write) (7, 3)
	// -> because loop wouldn't exit?
}
