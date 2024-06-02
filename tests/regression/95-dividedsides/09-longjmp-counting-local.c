// PARAM: --enable solvers.td3.narrow-sides.enabled --enable ana.int.interval --disable exp.volatiles_are_top
#include <setjmp.h>
#include <assert.h>
#include <goblint.h>

jmp_buf my_jump_buffer;
void foo(int count)
{
    __goblint_check(count >= 0 && count <= 5);
    longjmp(my_jump_buffer, 1);
}
int main(void)
{
    volatile int count = 0;
    setjmp (my_jump_buffer);
    if (count < 5) {
        count++;
        foo(count);
        __goblint_check(0); //NOWARN
    }
    __goblint_check(count == 5);
}