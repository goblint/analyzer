//PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'"
#include<stdlib.h>

void change_param(int* p){
	if(p != NULL)
		*p = 12;
}

void call_change_param(int *p){
	change_param(p);
}

// Test to check whether the write through call of call_change_param is handled
void test_call_change_param(){
	int x = 0;

	__goblint_check(x == 0);
	__goblint_check(x != 12);

	call_change_param(&x);

	__goblint_check(x != 0); //UNKNOWN
	__goblint_check(x == 12); //UNKNOWN
}