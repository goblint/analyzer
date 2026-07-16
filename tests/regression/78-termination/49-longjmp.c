// SKIP NONTERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <setjmp.h>
jmp_buf buf;
int main()
{
    if(setjmp(buf)) {

    }

    longjmp(buf, 1);
}
