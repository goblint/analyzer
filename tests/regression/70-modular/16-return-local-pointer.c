//SKIP PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'written'" --enable ana.int.interval

int *identity(int *p){
	return p;
}

int call_identity(){
	int x;
	int y;
	int *a = &x;

	__goblint_check(a == &x);
	__goblint_check(a != &y);
	a = identity(&y);
	__goblint_check(a == &y); //UNKNOWN
	__goblint_check(a != &x);
}
