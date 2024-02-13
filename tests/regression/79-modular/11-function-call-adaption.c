//PARAM: --set ana.modular.funs "['set_to_five', 'set_to_new_memory', 'set_to_z']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'read'" --set ana.activated[+] "'used_globals'" --set ana.activated[+] "'startstate'" --enable ana.int.interval
#include<goblint.h>
#include<stdlib.h>

void set_to_five(int *i){
	*i = 5;
}

void check_values_changed(){
	int i = 0;
	set_to_five(&i);
	__goblint_check(i <= 5);
	__goblint_check(i >= 0);
}

void set_to_new_memory(int **i){
	*i = malloc(sizeof(int));
}

int z;

void set_to_z(int **i){
	*i = &z;
}

void check_pointers_changed(){
	int j = 0;
	int *p = &j;

	__goblint_check(p == &j);
	set_to_new_memory(&p);
	__goblint_check(p != &j); //UNKNOWN
	__goblint_check(p != &z);
}

int main(){
	check_values_changed();
	check_pointers_changed();
	return 0;
}