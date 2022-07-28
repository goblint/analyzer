#include <assert.h>

int g=0;

void c()  __attribute__((__constructor__)) ;
void c(){
	g = 10;
}

int main(){
	assert(g==10);
	return 0;
}