#include <goblint.h>

int g=0;

void c()  __attribute__((__constructor__)) ;
void c(){
	g = 10;
}

int main(){
	__goblint_check(g==10);
	return 0;
}