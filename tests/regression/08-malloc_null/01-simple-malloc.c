// PARAM: --set ana.activated "[['base','escape','malloc_null']]"
#include <stdlib.h>

void *smalloc(size_t x){
        void * p = malloc(x);
        if (!p)
                exit(-1);
        return p;
}

void *no_malloc(size_t x){
        void * p = malloc(x);
        if (p)
                exit(-1);
        return p;
}

int main(void) {
        int *v;
        
        v = (int*)smalloc(sizeof(*v));
        *v = 10; // NOWARN
        
        
        v = (int*)malloc(sizeof(*v));
        if (v == 0){
                assert(0); // FAIL
  } else {
                assert(0); // FAIL
                *v != 0; // NOWARN
        }
        
        v = (int*)no_malloc(sizeof(*v));
        *v = 10; //WARN
        
        if (v == 0)
                exit(0);
 
        assert(0); // NOWARN

  return 0;
}
