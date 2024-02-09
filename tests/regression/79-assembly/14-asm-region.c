//PARAM: --set ana.activated[+] region --disable asm_is_nop
#include <stdlib.h>

int main() {
    char *buffer = malloc(256);  
    buffer[0] = 'a';            

    
    __asm__("movb $0, %0" : "=m" (*buffer));

    free(buffer);               
    return 0;
}
