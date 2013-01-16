#include <stdio.h>


FILE* f(){
	return fopen("test.txt", "a");  
}

int main(){
	FILE *fp1;
	FILE *fp2;
	fp1 = f();
	fp2 = f();	// Warn here: fp2 not closed
	fprintf(fp1, "Testing...\n");  
	fclose(fp1);
	fprintf(fp2, "Testing...\n");  
	// fclose(fp2);
}