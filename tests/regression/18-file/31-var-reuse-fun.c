#include <stdio.h>

FILE* myfopen(char* f){
	FILE* fp;
	fp = fopen(f, "a");
	return fp;
}

int main(){
	FILE *fp;
	fp = fopen("test1.txt", "a"); // WARN: file is never closed
	fp = myfopen("test2.txt"); // WARN: overwriting still opened file handle fp
	fclose(fp);
} // WARN: unclosed files: fp
