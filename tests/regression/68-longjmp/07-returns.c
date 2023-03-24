// PARAM: --enable ana.int.interval --enable ana.int.enums --disable exp.volatiles_are_top --set solvers.td3.side_widen never
#include <goblint.h>
#include <setjmp.h>

jmp_buf my_jump_buffer;

int foo(int count)
{
    __goblint_check(count >= 0 && count <= 5);
    longjmp(my_jump_buffer, -10);
    __goblint_check(0); // NOWARN
    return 8;
}

int main(void)
{
    volatile int count = 0;
    int x = setjmp(my_jump_buffer);
    if( x == -10) {
        __goblint_check(1);
        return -1;
    }
    if( x > 0) {
        __goblint_check(0); //NOWARN
    }

    foo(count);
}
