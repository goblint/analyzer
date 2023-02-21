#include <goblint.h>
#include <stdlib.h>

int main(){
	long long l;

	int* p = malloc(sizeof(int));

	int i;
	int* p2 = (int*) i; // create a top-pointer

	__goblint_check(p == p2); //UNKNOWN!

	if(p == p2){
		__goblint_check(p == p2); //TODO
	}

	return 0;
}