// SKIP PARAM: --enable ana.int.interval --enable ana.int.enums --enable exp.earlyglobs
#include <assert.h>
#include <setjmp.h>

jmp_buf my_jump_buffer;

int main(void)
{
    int count = setjmp(my_jump_buffer);
    assert(count == 0); // UNKNOWN!
    if (count < 5) {
        assert(count >= 0 && count < 5);
        longjmp(my_jump_buffer, count + 1);
        assert(0); // NOWARN
    }
    assert(count == 5);
}
