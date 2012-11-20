#include <stdio.h>

FILE *fp;

int main(){
  int b;
	fp = fopen("test.txt", "a");  
  
  if (b)
  	fclose(fp);  

	fprintf(fp, "Testing...\n"); // Warn here: fp might be closed?

  if (!b)
  	fclose(fp);    // may warn
}
