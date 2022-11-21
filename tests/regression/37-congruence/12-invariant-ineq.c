//PARAM: --enable ana.int.congruence --enable ana.int.interval --enable annotation.int.enabled
#include <goblint.h>

#define LIMIT 1000000

int foo() __attribute__ ((goblint_precision("def_exc","interval", "no-congruence")));
int foo(){
	unsigned int top;
	if(top > LIMIT){
		top = LIMIT;
	}

	if(top % 2 == 1){
		__goblint_check(top % 2 == 1); //UNKNOWN
	}

	if(top % 2 != 0){
		__goblint_check(top % 2 != 0); //UNKNOWN
	}
}

int main(){
	foo();
	unsigned int top;

	if(top > LIMIT){
		top = LIMIT;
	}

	if(top % 2 == 1){
		__goblint_check(top % 2 == 1);
	}

	if(top % 2 != 0){
		__goblint_check(top % 2 != 0);
	}

	if(top % 2 != 1){
		__goblint_check(top % 2 != 1);
	}

	if(top % 3 != 0){
		__goblint_check(top % 3 != 0); //TODO
	}

	unsigned int r;
	if(top > 0 && top < 10) {
		r = 3 * top;
		r = r;
	} else {
		r = 3;
		r = r;
	}

	if(r % 3 != 0) {
		// Unreachable
		__goblint_check(0); //NOWARN
	}

	if(r % 3 != 1) { } else {
		// Unreachable
		__goblint_check(0); //NOWARN
	}
	return 0;
}
