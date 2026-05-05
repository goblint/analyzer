// PARAM: --enable ana.arrayoob --enable ana.int.interval
#include <stdlib.h>

int main() {
    int size = 50;
    unsigned char *randomString = (unsigned char *)calloc(size, sizeof(unsigned char));

    randomString[17] = 42; //NOWARN

    free(randomString);
    return 0;
}
