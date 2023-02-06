// PARAM: --enable ana.int.interval --enable ana.int.enums --set solvers.td3.side_widen never
#include <setjmp.h>
#include <goblint.h>

int global = 0;
jmp_buf jmp_buffer;

int main(void)
{

    __goblint_check(1);
    setjmp(jmp_buffer);
    switch (global)
    {
    case 0:
        __goblint_check(1);
        global = 1;
        longjmp(jmp_buffer, 1);
        __goblint_check(0); // NOWARN
        break;
    case 1:
        __goblint_check(1);
        break;
    case 2:
        __goblint_check(0); // NOWARN
        break;
    default:
        __goblint_check(0); // NOWARN
        break;
    }
    __goblint_check(1);

    return 0;
}
