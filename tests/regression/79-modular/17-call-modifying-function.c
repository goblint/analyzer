//PARAM: --enable modular --set ana.modular.funs "['change_param', 'call_change_param']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'" --set ana.activated[+] "'used_globals'" --set ana.activated[+] "'startstate'"
#include<stdlib.h>

void change_param(int* p){
	if(p != NULL)
		*p = 12;
}

void call_change_param(int *p){
	change_param(p);
}

// Test to check whether the write through call of call_change_param is handled
void main(){
	int x = 0;

	__goblint_check(x == 0);
	__goblint_check(x != 12);

	call_change_param(&x);

	__goblint_check(x != 0); //UNKNOWN
	__goblint_check(x == 12); //UNKNOWN
}