//PARAM: --enable ana.int.interval --enable ana.int.enums --set solvers.td3.side_widen never --disable exp.volatiles_are_top
#include <goblint.h>
#include <setjmp.h>

jmp_buf my_jump_buffer;

int main(void)
{
    volatile int count = 0;
    setjmp(my_jump_buffer);
    __goblint_check(count == 0); // UNKNOWN!
    if (count < 5) {
        count++;
        __goblint_check(count >= 0 && count <= 5);
        longjmp(my_jump_buffer, 1);
        __goblint_check(0); // NOWARN
    }
    __goblint_check(count == 5);
}
