//PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'"
#include<stdlib.h>
#include<goblint.h>

int g;
int *h = NULL;

void set_global_to(int* p){
	h = p;
}

void call_set_global_to(int *p){
	set_global_to(p);
}

// Test to check whether the write through call of call_change_param is handled
void test_call_change_param(){
	int x = 0;

	__goblint_check(h != &x);
	call_set_global_to(&x);
	__goblint_check(h == &x); //UNKNOWN
}
