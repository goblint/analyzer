#include <stdio.h>


FILE* myfopen(char* f){
	return fopen(f, "a");  
}

int main(){
	FILE *fp1;
	FILE *fp2;
	fp1 = myfopen("test1.txt");
	fp2 = myfopen("test2.txt");	// WARN: file is never closed

	fprintf(fp1, "Testing...\n");  
	fclose(fp1);
	fprintf(fp2, "Testing...\n");  
	// fclose(fp2);
}