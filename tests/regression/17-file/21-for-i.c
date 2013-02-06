#include <stdio.h>

FILE *fp;

int main(){
	int i;
	fp = fopen("test.txt", "w");  // Warn here: fp might not be closed?

	for(i=1; i<10; i++){ // join
		// i -> Unknown int
		if(i%2){
			// i -> Unknown int
			// fprintf(fp, "Testing...%s\n", i); // Segmentation fault!
			fprintf(fp, "Testing...%i\n", i);
			fclose(fp);  
		}else{
			fp = fopen("test.txt", "a");
		}
		// why no join?
	}
	// fp opened or closed? (last i=9 -> open)
	// widening -> Warn: might be unclosed
}
