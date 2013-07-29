#include <stdio.h>

FILE *fp;

int main(){
	int i;
	fp = fopen("test.txt", "a");  // WARN: file is never closed

	while (i){
		fprintf(fp, "Testing...\n");
		i++;
	}

	//fclose(fp);  
} // WARN: unclosed files: fp
