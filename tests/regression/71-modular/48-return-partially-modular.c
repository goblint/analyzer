//PARAM: --set ana.modular.funs "['foo']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'" --enable ana.int.interval

#include<goblint.h>

// This function should only be analyzed once
int *foo(int *i){
	*i = 42;
	return i;
}

int main(){
	int x = 4;
	int y = 4;
	__goblint_check(x == 4);
	__goblint_check(y == 4);

	y = *foo(&x);
	__goblint_check(x == 42); //UNKNOWN
	__goblint_check(x != 4); //UNKNOWN
	__goblint_check(x >= 4);
	__goblint_check(x <= 42);

	__goblint_check(y == 42); //UNKNOWN
	__goblint_check(y != 4); //UNKNOWN
	__goblint_check(y >= 4);
	__goblint_check(y <= 42);

	x = 1;
	y = *foo(&x);
	__goblint_check(x == 42); //UNKNOWN
	__goblint_check(x != 1); //UNKNOWN
	__goblint_check(x >= 1);
	__goblint_check(x <= 42);

	__goblint_check(y == 42); //UNKNOWN
	__goblint_check(y != 1); //UNKNOWN
	__goblint_check(y >= 1);
	__goblint_check(y <= 42);

}