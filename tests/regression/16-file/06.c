#include <stdio.h>

FILE *fp;

int main(){
	fp = fopen("test1.txt", "a"); // Warn here: fp not closed 
	fp = fopen("test2.txt", "a"); 
	fprintf(fp, "Testing...\n"); 
	fclose(fp);
}