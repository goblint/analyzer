// PARAM: --set ana.modular.funs "['let_escape']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'"
#include<stdlib.h>
#include<pthread.h>

int g = 0;
int *p1 = NULL;
int *p2 = NULL;

void let_escape(){
	int x = 0;
	int *local_ptr = &x;

	__goblint_check(p1 != local_ptr);
	__goblint_check(p2 != local_ptr);

	p2 = local_ptr;

	int *p1_copy = p1;
	int *p2_copy = p2;
	__goblint_check(p1 != local_ptr); //UNKNOWN!
	__goblint_check(p2 != local_ptr); //UNKNOWN!

	__goblint_check(p1_copy != local_ptr); //UNKNOWN!
	__goblint_check(p2_copy != local_ptr); //UNKNOWN!

}

void *change_ptrs(void *v){
	__goblint_check(p1 == p2); //UNKNOWN!
	p1 = p2;
	p2 = NULL;
}

int main(){
	pthread_t thread;
	pthread_create(&thread, NULL, change_ptrs, NULL);

	let_escape();
}

