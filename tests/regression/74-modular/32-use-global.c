//PARAM: --set ana.modular.funs "['compare_to_global_addr', 'call_compare_to_global_addr']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'" --enable ana.int.interval

#include<goblint.h>
#include<stdlib.h>

int g = 0;
int h = 0;

int compare_to_global_addr(int *p){
	if(p == &g){
		h = 1;
	}
}

int call_compare_to_global_addr(int *p){
	compare_to_global_addr(p);
}

int main(){
	int *p = &g;
	call_compare_to_global_addr(p);
	__goblint_check(h == 1); //UNKNOWN
}