//PARAM: --enable modular --enable ana.modular.auto-funs --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'" --set ana.activated[+] "'used_globals'" --set ana.activated[+] "'startstate'"

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
