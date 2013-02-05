#include <stdio.h>

FILE *fp;

int main(){
	int i;
	fp = fopen("test.txt", "w");  // Warn here: fp might not be closed?

	for(i=1; i<10; i++){
		if(i%2){
			// fprintf(fp, "Testing...%s\n", i); // Segmentation fault!
			fprintf(fp, "Testing...%i\n", i);
			fclose(fp);  
		}else{
			fp = fopen("test.txt", "a");
		}
	}

}
