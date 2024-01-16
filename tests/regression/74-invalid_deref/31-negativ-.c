// PARAM: --set ana.activated[+] memOutOfBounds  --enable ana.int.interval
#include <stdlib.h>

int main(int argc, char const *argv[]) {
    char *ptr = malloc(5 * sizeof(char));
    
    for (int i = 0; i < 5; i++) {
        ptr[i] = i; //NOWARN
        ptr[-i] = i; //WARN
    }

    ptr[-1] =2;; //WARN
    ptr[-2]= 2;; //WARN
    ptr[-3]= 3; //WARN

    ptr[1]; //NOWARN
    ptr[2]; //NOWARN
    ptr[3]; //NOWARN

    free(ptr); 

    return 0;
}
