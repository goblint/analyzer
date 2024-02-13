//PARAM: --enable modular --enable ana.modular.auto-funs --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'" --set ana.activated[+] "'used_globals'" --set ana.activated[+] "'startstate'"

void write_into_array(int *a, int size){
	a[size - 1] = 123;
}

void test_array(){
	int size = 10;
	int a[size];

	for(int i = 0; i < size; i++){
		a[i] = 0;
	}

	__goblint_check(a[0] == 0);
	__goblint_check(a[size - 1] == 0);

	write_into_array(a, size);

	__goblint_check(a[size - 1] != 0); //UNKNOWN
	__goblint_check(a[size - 1] == 123); //UNKNOWN

}