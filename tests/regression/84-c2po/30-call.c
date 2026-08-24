// PARAM: --set ana.activated[+] c2po --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts --set ana.c2po.askbase false

#include <goblint.h>

int g;
int h;

int *foo(int *p, int *q){
	// Improved treatment of function calls required for this assertion
	__goblint_check(p == q); // TODO
	return p;
}

int main(){
	int *ptr = &g;
	int *ptr2 = &g;
	int top;
	if (top){
		ptr = &h;
		ptr2 = &h;
	}
	__goblint_check(ptr == ptr2);
	int* ret = foo(ptr, ptr);
	__goblint_check(ptr == ret);
	return 0;
}