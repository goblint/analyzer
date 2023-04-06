//PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'written'" --enable ana.int.interval

void set_to_five(int *i){
	*i = 5;
}

void bar(){
	int i = 0;
	set_to_five(&i);
	__goblint_check(i <= 5);
	__goblint_check(i >= 0);
}