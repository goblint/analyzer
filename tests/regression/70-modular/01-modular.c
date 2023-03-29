//PARAM: --enable modular

#include <goblint.h>

void foo(){
	// Check that the modular analysis analyizes function even when not called from main
	__goblint_check(1);
}

void bar(){
	// Check that the modular analysis analyizes function even when not called from main
	__goblint_check(1);
}