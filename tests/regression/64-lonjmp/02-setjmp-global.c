// PARAM: --enable ana.int.interval --enable ana.int.enums --set solvers.td3.side_widen never
#include <setjmp.h>
#include <assert.h>

int global = 0;

int main(void)
{
    jmp_buf jmp_buf;

    assert(1);
    setjmp(jmp_buf);
    switch (global)
    {
    case 0:
        assert(1);
        global = 1;
        longjmp(jmp_buf, 1);
        assert(0); // NOWARN
        break;
    case 1:
        assert(1);
        break;
    case 2:
        assert(0); // NOWARN
        break;
    default:
        assert(0); // NOWARN
        break;
    }
    assert(1);

    return 0;
}
