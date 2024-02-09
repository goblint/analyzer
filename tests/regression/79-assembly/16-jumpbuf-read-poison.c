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
	
		asm ("nop" : "x="(i):"x="(i)); //WARN
	} else {
	
		printf("Setting i to 69; %d\n", i);
		i = 10;
		test();
	}
}
