// PARAM: --enable ana.int.interval --enable ana.int.enums --set solvers.td3.side_widen never
#include <assert.h>
#include <setjmp.h>

jmp_buf my_jump_buffer;
int count = 0;

void foo()
{
    assert(count >= 0 && count < 5);
    count++;
    longjmp(my_jump_buffer, 1);
    assert(0); // NOWARN
}

int main(void)
{
    setjmp(my_jump_buffer);
    assert(count == 0); // UNKNOWN!
    if (count < 5) {
        foo();
        assert(0); // NOWARN
    }
    assert(count == 5);
}
