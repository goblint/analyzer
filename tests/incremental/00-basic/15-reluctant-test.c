#include <goblint.h>
// This test used to resulted in an unreachd fixpoint in the incremental implementation.

int g = 3;

int bar(){
	int x = foo();
	return x;
}

int foo(){
	int top;
	int r = 0;
	if(top){
		r = g;
	} else {
		// bar();
		// r = 5;
	}
	return r;
}

int main(){
	int x;

	x = foo();
	if(x == 5){
		g = 4;
		int i = bar();


	}
	return x;
}