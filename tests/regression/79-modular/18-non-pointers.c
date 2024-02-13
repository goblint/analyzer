//PARAM: --enable modular --set ana.modular.funs "['write_into_array']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'" --set ana.activated[+] "'used_globals'" --set ana.activated[+] "'startstate'"

void write_into_array(int *a, int value){
	*a = value;
}

void main(){
	int value = 10;
	int i = 0;
	int *a = &i;

	__goblint_check(*a == 0);
	write_into_array(a, value);
	__goblint_check(*a != 0); // UNKNOWN
}