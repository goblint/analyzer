// PARAM: --set ana.activated[+] memOutOfBounds --enable ana.int.interval --set ana.activated[+] apron --set ana.apron.domain polyhedra --enable ana.apron.pointer_tracking  --set sem.int.signed_overflow assume_none --disable warn.integer
#include <stdlib.h>
int main()
{
    int len;
    long *ptr;
    scanf("%d", &len);

    if (len < 0)
    {
        return 0;
    }

    ptr = malloc(sizeof(long) * len);

    long *ptr3 = ptr;
    long *ptr4 = ptr;

    // take the address of ptr3
    long *t = (&ptr3) + 0;
    *t = *t + 0;

    long *t2 = (&ptr4) + 1;
    *t2 = *t2 + 0;

    for (int i = 0; i < len; i++)
    {
        long s = *ptr3; // UNKWOWN
        s = *ptr4;      // UNKWOWN
        s = *ptr;       // NOWARN

        ptr3 = ptr3 + 1;
        ptr4 = ptr4 + 1;
        ptr = ptr + 1;
    }

    // reset pointer to start of new malloc block
    ptr = malloc(sizeof(int) * len);

    int *ptr5 = ptr;
    int *ptr6 = ptr;

    int *t3 = (int)((&ptr5) + 0);
    *t3 = *t3 - 1;

    for (int i = 0; i < len; i++)
    {
        *ptr5 = 2;     // UNKWON
        int s = *ptr6; // NOWARN
        s = *ptr;      // NOWAWN

        ptr5 = ptr5 + 1;
        ptr6 = ptr6 + 1;
        ptr = ptr + 1;
    }

    // reset pointer to start of new malloc block
    ptr = malloc(sizeof(int) * len);
    ptr6 = ptr;

    int *t4 = (rand() ? (&ptr6) : 0);
    // invalidates all pointer relations as the dereferenced pointer does not has cardinality 1 (or is unknown)
    *t4 = *t4 - 1;

    for (int i = 0; i < len; i++)
    {
        int s = *ptr6; // UNKWON
        s = *ptr;      // UNKWON

        ptr6 = ptr6 + 1;
        ptr = ptr + 1;
    }
}
