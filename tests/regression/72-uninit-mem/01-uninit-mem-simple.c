#include <stdlib.h>
#include <stdio.h>

int main(int argc, char const *argv[])
{
    char *ptr = malloc(5 * sizeof(char));
    char *ptr2 = "str";
    printf("%s\n", ptr); // Should WARN that ptr points to uninitd. mem.

    ptr = ptr2;
    printf("%s\n", ptr); // Now, it shouldn't WARN that ptr points to uninitd. mem.
    return 0;
}
