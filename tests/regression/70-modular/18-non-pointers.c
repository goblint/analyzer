//PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'"

int write_into_array(int *a, int value){
	*a = value;
}

int test_array(){
	int value = 10;
	int i = 0;
	int *a = &i;

	__goblint_check(*a == 0);
	write_into_array(a, value);
	__goblint_check(*a != 0); // UNKNOWN
}