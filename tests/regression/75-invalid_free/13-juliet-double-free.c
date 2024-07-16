//PARAM: --set ana.activated[+] useAfterFree
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <wchar.h>

typedef struct _twoIntsStruct {
    int intOne;
    int intTwo;
} twoIntsStruct;

static int staticTrue = 1; /* true */
static int staticFalse = 0; /* false */

void CWE415_Double_Free__malloc_free_struct_05_bad()
{
    twoIntsStruct * data;
    data = NULL;
    if(staticTrue)
    {
        data = (twoIntsStruct *)malloc(100*sizeof(twoIntsStruct));
        if (data == NULL) {exit(-1);}
        free(data);
    }
    if(staticTrue)
    {
        free(data); //WARN (Double Free (CWE-415))
    }
}

static void goodB2G1()
{
    twoIntsStruct * data;
    data = NULL;
    if(staticTrue)
    {
        data = (twoIntsStruct *)malloc(100*sizeof(twoIntsStruct));
        if (data == NULL) {exit(-1);}
        free(data);
    }
}

static void goodB2G2()
{
    twoIntsStruct * data;
    data = NULL;
    if(staticTrue)
    {
        data = (twoIntsStruct *)malloc(100*sizeof(twoIntsStruct));
        if (data == NULL) {exit(-1);}
        free(data);
    }
}

static void goodG2B1()
{
    twoIntsStruct * data;
    data = NULL;
    if(staticFalse)
    {
        /* INCIDENTAL: CWE 561 Dead Code, the code below will never run */
        printf("%s\n", "Benign, fixed string");
    }
    else
    {
        data = (twoIntsStruct *)malloc(100*sizeof(twoIntsStruct));
        if (data == NULL) {exit(-1);}
    }
    if(staticTrue)
    {
        free(data);
    }
}

static void goodG2B2()
{
    twoIntsStruct * data;
    data = NULL;
    if(staticTrue)
    {
        data = (twoIntsStruct *)malloc(100*sizeof(twoIntsStruct));
        if (data == NULL) {exit(-1);}
    }
    if(staticTrue)
    {
        free(data);
    }
}

void CWE415_Double_Free__malloc_free_struct_05_good()
{
    goodB2G1();
    goodB2G2();
    goodG2B1();
    goodG2B2();
}

int main(int argc, char * argv[])
{
    CWE415_Double_Free__malloc_free_struct_05_good();
    CWE415_Double_Free__malloc_free_struct_05_bad();

    return 0;
}