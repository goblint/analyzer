// SKIP PARAM: --set ana.activated[+] apron --set ana.relation.privatization top
 
#include <pthread.h>
#include <goblint.h>

    int num = 1;


int main() {

    while(num > 0)
        num++; // Here num overflows

    __goblint_check(1); // reachable
    
    return 0;
}
