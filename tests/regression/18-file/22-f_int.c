// PARAM: --set ana.activated[0][+] "'file'" --enable  ana.file.optimistic

#include <stdio.h>

int f(int x){
	return 2*x;
}

int main(){
	int a = 1;
	a = f(2);
	return 0;
}
