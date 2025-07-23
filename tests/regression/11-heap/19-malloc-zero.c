// PARAM: --set sem.malloc.zero either --set ana.activated[+] memOutOfBounds 
#include<pthread.h>
#include<goblint.h>

int main(void){
  int* ptr = malloc(0);

  if(ptr == 0) {
    // Reachable
    __goblint_check(1);
  } else {
    *ptr = 1; //WARN
  }
}
