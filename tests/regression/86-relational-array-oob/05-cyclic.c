// PARAM: --enable ana.arrayoob --set ana.activated[+] apron  --set ana.apron.domain octagon  --enable ana.int.interval --set ana.activated[+] allocVarEscaped --disable warn.integer  --set ana.activated[+] taintPartialContexts
#include <stdio.h>

int len;

void foo1(int c, int gptr[])
{

    for (int i = 0; i < len; i++)
    {
        gptr[i] = 42; // NOWARN
        // This is quite weird both assignments the above and below call eval_offset,
        // however for the below we get "any_index" as the offset, which causes a warning to be generated.
        // This only happens in function calls
        int tmp = gptr[i]; // TODO NOWARN
    }
    if (c < 10)
        foo2(c + 1, gptr); // for some reason activating taintPartialContexts causes ArrayOutOfBounds warnings to be generated for function call

    // deactivating causes those warnings the NOWARN to fail
    for (int i = 0; i < len; i++)
    {
        gptr[i] = 42;      // NOWARN
        int tmp = gptr[i]; // TODO NOWARN
    }
}

void foo2(int c, int gptr[])
{
    for (int i = 0; i < len; i++)
    {
        gptr[i] = 42; // NOWARN
        int tmp = gptr[i]; // TODO NOWARN
    }
    if (c < 10)
        foo1(c + 1, gptr); // for some reason activating taintPartialContexts causes ArrayOutOfBounds warnings to be generated for function call
    for (int i = 0; i < len; i++)
    {
        gptr[i] = 42;      // NOWARN
        int tmp = gptr[i]; // TODO  NOWARN
    }
}

int main()
{
    int myInt;
    scanf("%d", &myInt);

    if (myInt <= 0)
    {
        myInt = 1;
    }
    len = myInt;

    int gptr[len];

    foo1(0, gptr); // for some reason activating taintPartialContexts causes ArrayOutOfBounds warnings to be generated for function call

    for (int i = 0; i < len; i++)
    {
        gptr[i] = 42; // NOWARN
        // We don't get "any_index" as the offset here
        int tmp = gptr[i]; // NOWARN
    }
    return 0;
}
