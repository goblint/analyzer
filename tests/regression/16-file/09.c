#include <stdio.h>

FILE *fp;

int main(){
  int i;
	fp = fopen("test.txt", "a");  // Warn here: fp might not be closed?
  
	while (i){
    fprintf(fp, "Testing...\n");
    i++;
  }
  
	//fclose(fp);  
}
