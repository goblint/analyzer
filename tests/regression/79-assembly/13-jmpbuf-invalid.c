// PARAM: --set asm_is_nop false
#include <setjmp.h>

jmp_buf buf;

int main(void) {

    if (!setjmp(buf)) {

        __asm__("nop" : "=m"(buf));

        longjmp(buf, 1); //WARN
    }
}