//PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'written'" --enable ana.int.interval

void change_param(int* p){
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