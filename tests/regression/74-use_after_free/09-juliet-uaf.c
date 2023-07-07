//PARAM: --set ana.activated[+] useAfterFree
#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>

static char * helperBad(char * aString)
{
    size_t i = 0;
    size_t j;
    char * reversedString = NULL;
    if (aString != NULL)
    {
        i = strlen(aString);
        reversedString = (char *) malloc(i+1);
        if (reversedString == NULL) {exit(-1);}
        for (j = 0; j < i; j++)
        {
            reversedString[j] = aString[i-j-1];
        }
        reversedString[i] = '\0';

        free(reversedString);
        return reversedString; // WARN (Use After Free (CWE-416))
    }
    else
    {
        return NULL;
    }
}

static char * helperGood(char * aString)
{
    size_t i = 0;
    size_t j;
    char * reversedString = NULL;
    if (aString != NULL)
    {
        i = strlen(aString);
        reversedString = (char *) malloc(i+1);
        if (reversedString == NULL) {exit(-1);}
        for (j = 0; j < i; j++)
        {
            reversedString[j] = aString[i-j-1];
        }
        reversedString[i] = '\0';
        return reversedString; //NOWARN
    }
    else
    {
        return NULL;
    }
}

static int staticReturnsTrue()
{
    return 1;
}

static int staticReturnsFalse()
{
    return 0;
}

void CWE416_Use_After_Free__return_freed_ptr_08_bad()
{
    if(staticReturnsTrue())
    {
        {
            char * reversedString = helperBad("BadSink"); // WARN (Use After Free (CWE-416))
            printf("%s\n", reversedString); // WARN (Use After Free (CWE-416))
        }
    }
}

static void good1()
{
    if(staticReturnsFalse())
    {
        /* INCIDENTAL: CWE 561 Dead Code, the code below will never run */
        printf("%s\n", "Benign, fixed string");
    }
    else
    {
        {
            char * reversedString = helperGood("GoodSink");
            printf("%s\n", reversedString);
        }
    }
}

static void good2()
{
    if(staticReturnsTrue())
    {
        {
            char * reversedString = helperGood("GoodSink");
            printf("%s\n", reversedString);
        }
    }
}

void CWE416_Use_After_Free__return_freed_ptr_08_good()
{
    good1();
    good2();
}


int main(int argc, char * argv[])
{
    CWE416_Use_After_Free__return_freed_ptr_08_good();
    CWE416_Use_After_Free__return_freed_ptr_08_bad();

    return 0;
}