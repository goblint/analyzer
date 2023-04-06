//SKIP PARAM: --enable modular --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'written'" --enable ana.int.interval
#include<goblint.h>
#include<stdlib.h>

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

	int *zp = &z;
	set_to_z(&p);

	__goblint_check(p != &j); //UNKNOWN
	__goblint_check(p != &z); //UNKNOWN
	__goblint_check(p != zp); //UNKNOWN
}