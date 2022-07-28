// PARAM: --set ana.activated "['base','threadid','threadflag','escape','mallocWrapper','assert']" --set ana.base.privatization none
#include <assert.h>

extern void f_everything_up();

struct s {
	void (*f)(void);
	int data;
} s;

void hello(){
	//is it me your looking for ...
	__goblint_check(1);
}

int g = 0;
void (*fp)(void) = &hello;

int main(){
	s.f = &hello;
	__goblint_check(s.f == &hello);
	__goblint_check(fp == &hello);
	f_everything_up();
	s.f();
	__goblint_check(s.data == 0); // UNKNOWN!!1!one!
	__goblint_check(fp == &hello);// UNKNOWN
	return 0;
}
