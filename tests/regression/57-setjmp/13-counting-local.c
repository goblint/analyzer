// PARAM: --enable ana.int.interval --enable ana.int.enums --set "ana.activated[+]" setjmp --set solvers.td3.side_widen never --disable exp.volatiles_are_top
#include <assert.h>
#include <setjmp.h>

jmp_buf my_jump_buffer;

void foo(int count)
{
    assert(count >= 0 && count <= 5);
    longjmp(my_jump_buffer, 1);
    assert(0); // NOWARN
}

int main(void)
{
    volatile int count = 0;
    setjmp(my_jump_buffer);
    assert(count == 0); // UNKNOWN!
    if (count < 5) {
        count++;
        foo(count);
        assert(0); // NOWARN
    }
    assert(count == 5);
}