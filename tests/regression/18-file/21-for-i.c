#include <stdio.h>

FILE *fp;

int main(){
	int i;
	fp = fopen("test.txt", "w"); // WARN: MAYBE file is never closed

	for(i=1; i<10; i++){ // join
		// i -> Unknown int
		if(i%2){
			// i -> Unknown int
			// fprintf(fp, "Testing...%s\n", i); // Segmentation fault!
			// actually shouldn't warn because open and close are always alternating...
			fprintf(fp, "Testing...%i\n", i); // WARN: MAYBE writing to closed file handle fp
			fclose(fp); // WARN: MAYBE closeing already closed file handle fp
		}else{
			fp = fopen("test.txt", "a"); // WARN: MAYBE file is never closed
		}
		// why no join?
	}
	// fp opened or closed? (last i=9 -> open)
	// widening -> Warn: might be unclosed
} // WARN: MAYBE unclosed files: fp
