#include <stdio.h>


FILE* myfopen(){
	return fopen("test.txt", "a");  
}

int main(){
	FILE *fp1;
	FILE *fp2;
	fp1 = myfopen();
	fp2 = myfopen();	// WARN: file is never closed

	fprintf(fp1, "Testing...\n");  
	fclose(fp1);
	fprintf(fp2, "Testing...\n");  
	// fclose(fp2);
}