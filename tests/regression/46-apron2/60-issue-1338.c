// SKIP PARAM: --set ana.activated[+] apron
#include <stdlib.h>
int main()
{
    char *ptr = malloc(2);
    char s = *(ptr+0)+0;

    char *arr;
    arr = malloc(8);
    int tmp = (int)*(arr+0);
}