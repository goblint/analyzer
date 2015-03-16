// PARAM: --set ana.activated[+] "'file'" --enable  ana.file.optimistic

#include <stdio.h>

int main (){
	FILE *fp;
	fp = fopen("test.txt", "w"); // WARN: MAYBE file is never closed

	if(fp==NULL){
		fprintf(fp, "Testing..."); // WARN: writing to unopened file handle fp
		fclose(fp); // WARN: closeing unopened file handle fp
	}
} // WARN: MAYBE unclosed files: fp
