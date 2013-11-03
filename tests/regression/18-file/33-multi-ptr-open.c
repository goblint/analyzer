#include <stdio.h>

int main(){
  	FILE *fp1;
	fp1 = fopen("test1.txt", "a"); // WARN: MAYBE file is never closed

	FILE *fp2;
	fp2 = fopen("test2.txt", "r"); // WARN: MAYBE file is never closed
	
  	FILE **fp;
  	int b;
    if(b){
      fp = &fp1;
    }else{
      fp = &fp2;
    }

  	fprintf(*fp, "Testing...\n"); // WARN: MAYBE writing to read-only file handle fp

  	fclose(*fp);
} // WARN: MAYBE unclosed files: fp1, fp2