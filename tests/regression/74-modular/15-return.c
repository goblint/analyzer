//PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'" --enable ana.int.interval

#include<goblint.h>
#include<stdlib.h>

int *new_int(){
	int *new = malloc(sizeof(int));
	return new;
}

int g;

int call_new_int(){
	int z;
	int *a = &z;

	__goblint_check(a == &z);
	__goblint_check(a != &g);
	a = new_int();
	__goblint_check(a != &z);
	__goblint_check(a != &g);
}

int *get_g_pointer(){
	return &g;
}

int call_get_g_pointer(){
	int z;
	int *a = &z;

	__goblint_check(a == &z);
	__goblint_check(a != &g);
	a = get_g_pointer();
	__goblint_check(a == &g); //UNKNOWN
	__goblint_check(a != &z);
}

