// PARAM: --set ana.activated[+] "'file'" --enable  ana.file.optimistic --disable warn.info

#include <stdio.h>

int main (){
	FILE *fp;
	fp = fopen("test.txt", "w"); // NOWARN

	if(fp!=NULL){
		fprintf(fp, "Testing..."); // NOWARN
		fclose(fp); // NOWARN
	}
} // NOWARN

// All ok!
