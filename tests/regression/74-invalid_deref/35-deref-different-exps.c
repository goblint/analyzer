// SKIP PARAM: --set ana.activated[+] memOutOfBounds --set ana.activated[+] apron  --set ana.apron.domain octagon
// The purpose of this file is to test different deref expressions, which were previously not tested
#include <stdlib.h>
#include <complex.h> 

int main()
{
    int len = 5;
    int *gptr = (int *)malloc(sizeof(int) * len);

    // deref Cast
    int t = *((int *)len);

    // deref Question
    int tmp = (len > 0) ? 0 : gptr[len - 1];
    int condition = 1;
    int trueVal = 7, falseVal = 8;
    int ptrCondition = *(condition ? &trueVal : &falseVal);

    // AddrOfLabel (GCC extension)
    void *labelPtr = &&labelTarget;
    goto *labelPtr;
labelTarget:;

    free(gptr);
    return 0;
}