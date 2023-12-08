#include <stdlib.h>

int main(int argc, char const *argv[])
{
    int a;
    int *p = &a;
    free(p); //WARN

    char b = 'b';
    char *p2 = &b;
    free(p2); //WARN

    return 0;
}
