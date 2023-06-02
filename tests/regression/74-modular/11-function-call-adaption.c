//PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'" --enable ana.int.interval
#include<goblint.h>
#include<stdlib.h>

void set_to_five(int *i){
	*i = 5;
}

void bar(){
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

void foo(){
	int j = 0;
	int *p = &j;

	__goblint_check(p == &j);
	set_to_new_memory(&p);
	__goblint_check(p != &j); //UNKNOWN
	__goblint_check(p != &z);
}
