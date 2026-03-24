//PARAM: --set solver bu --enable ana.int.interval --disable ana.base.context.int
#include <stdio.h>
#include <goblint.h>

int foo(int* ptr, int z){
	__goblint_check(z < 11); // TODO
	int r = identity(z);
	__goblint_check(z < 11); // TODO
	return r;
}

int identity(int z){
	return z;
}

int main(){
	int a = 0;
	int i;
	for(i = 0; i < 10; i++){
		printf("Iteration %d\n", i);
	}
	int *ptr = &i;
	if (i == 34){
		ptr = & a;
	}
	int h = foo(ptr, i);
	int x = identity(0);
	__goblint_check(x < 11); //TODO
}