//PARAM: --set ana.modular.funs "['foo']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'" --enable ana.int.interval

#include<goblint.h>

// This function should only be analyzed once
int foo(int *i){
	*i = 42;
}

int main(){
	int x = 4;
	__goblint_check(x == 4);
	foo(&x);
	__goblint_check(x == 42); //UNKNOWN
	__goblint_check(x != 4); //UNKNOWN

	x = 1;
	foo(&x);
	__goblint_check(x == 42); //UNKNOWN
	__goblint_check(x != 1); //UNKNOWN

	x = 2;
	foo(&x);
	__goblint_check(x == 42); //UNKNOWN
	__goblint_check(x != 2); //UNKNOWN
}