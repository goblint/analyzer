// PARAM: --set ana.activated[+] "'file'" --enable  ana.file.optimistic

#include <stdio.h>

char* f(char* x){
	return x;
}

int main(){
	char* a = "foo";
	a = f("bar");
	return 0;
}
