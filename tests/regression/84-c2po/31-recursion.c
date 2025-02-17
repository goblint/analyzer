// PARAM: --set ana.activated[+] c2po --set ana.activated[+] startState --set ana.activated[+] taintPartialContexts --set ana.c2po.askbase false

#include <goblint.h>

int g;
int h;


int foo2(int x2, int *ptr2){
	int y2 = x2;
	if (x2 < 3){
		// foo2(x + 1, &g);
		__goblint_check(1);
	}

	return x2;
}

int foo(int x, int *ptr){
	int y = x;
	if (x < 3){
		foo2(x + 1, &g);
		__goblint_check(1);
	}
	return x;
}

int main(){
	foo(1, &h);
	return 0;
}