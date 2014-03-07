// PARAM: --set ana.activated[0][+] "'file'" --enable  ana.file.optimistic

#include <stdio.h>
#include <stddef.h>

wchar_t* f(wchar_t* x){
	return x;
}

int main(){
	wchar_t* a = L"foo";
	a = f(L"bar");
	return 0;
}
