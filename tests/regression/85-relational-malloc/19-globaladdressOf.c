// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --set ana.activated[+] apron --set ana.apron.domain polyhedra --enable ana.apron.pointer_tracking  --set sem.int.signed_overflow assume_none --disable warn.integer --enable ana.sv-comp.functions
#include <stdlib.h>
extern int __VERIFIER_nondet_int(void);
int *ptr;
int *ptr2;
int *ptr3;

int f(int len)
{
    int t = &ptr;
}

int main()
{
    int len = __VERIFIER_nondet_int();
    if (len < 1 || len >= 2147483647 / sizeof(char))
    {
        len = 1;
    }
    ptr = malloc(sizeof(char) * len);
    ptr2 = malloc(sizeof(int) * len);
    f(len);
    // invalide ptr relation after function call

    for (int i = 0; i < len; i++)
    {
        int s = *ptr;  // WARN
        int s = *ptr2; // NOWARN
        ptr = ptr + 1;
        ptr2 = ptr2 + 1;
    }

    ptr = malloc(sizeof(char) * len);
    ptr2 = malloc(sizeof(int) * len);

    if (rand())
    {
        int t = &ptr3;
    }
    else
    {
        int t = &ptr3 + 1;
    }

    for (int i = 0; i < len; i++)
    {
        int s = *ptr;  // UNKWOWN
        int s = *ptr2; // NOWARN
        ptr = ptr + 1;
        ptr2 = ptr2 + 1;
    }
}