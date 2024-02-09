// PARAM: --disable asm_is_nop
#include <stdio.h>
#include <setjmp.h>

jmp_buf buf;

void test(void) {

	longjmp(buf, 1);
}

int main(void) {

	int i = 0;

	if (setjmp(buf)){
	
		printf("After Jump: %d\n",i); //WARN
	} else {
	
		printf("Setting i to 69; %d\n", i);
		asm ("nop" : "=x"(i) : "=x"(i));
		test();
	}
}
