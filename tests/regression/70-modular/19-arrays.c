//PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'"

int write_into_array(int *a, int size){
	a[size - 1] = 123;
}

int test_array(){
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