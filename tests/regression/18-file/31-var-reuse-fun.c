#include <stdio.h>

FILE* myfopen(char* f){
  	FILE* fp;
  	fp = fopen(f, "a");
  	return fp;
}

int main(){
	FILE *fp;
  	fp = fopen("test1.txt", "a"); // fp is now in D
    // also should warn about not closed and overwriting...
	fp = myfopen("test2.txt"); // is myfopen::fp now treated as a global and aliased?
	fclose(fp);
}

// All ok!
