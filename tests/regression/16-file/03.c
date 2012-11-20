#include <stdio.h>

FILE *fp;

int main(){
  int b;
	fp = fopen("test.txt", "a");  // Warn here: fp might not be closed?
  
	fprintf(fp, "Testing...\n");
  
  if (b)
  	fclose(fp);  
}
