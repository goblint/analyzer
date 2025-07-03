// PARAM: --set sem.malloc.zero either
#include<pthread.h>
#include<goblint.h>

int main(void){
  int* ptr = malloc(0);

  if(ptr == 0) {
    // Reachable
    __goblint_check(1);
  }
}
