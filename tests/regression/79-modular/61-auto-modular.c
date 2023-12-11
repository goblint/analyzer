//PARAM: --enable modular --enable ana.modular.auto-funs --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'" --set ana.activated[+] "'used_globals'"

#include<goblint.h>

int g;
int h;

typedef int int_ptr_to_int(int*);

int bar(int *i){
	__goblint_check(i == &g); //UNKNOWN
	__goblint_check(i == &h); //UNKNOWN
	return i;
}

int foo(int_ptr_to_int *f){
	int *i = &g;
	return f(i);
}

int main(){
	foo(bar);
}




