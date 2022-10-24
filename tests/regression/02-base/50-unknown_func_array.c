#include <stdlib.h>
#include <stdio.h>
#include <goblint.h>

#define LENGTH 10

typedef struct arr {
    int *ptrs[LENGTH];
} arr_t;

// int mutate_array(arr_t a){
//     int t = rand();
//     for(int i=0; i<LENGTH; i++) {
//         *(a.ptrs[i]) = t;
//     }
// }

int main(){
    arr_t a;
    int xs[LENGTH];

    for(int i=0; i<LENGTH; i++){
        xs[i] = 0;
        a.ptrs[i] = &xs[0];
    }

    // When passing an arrays to an unknown function, reachable memory should be invalidated
    mutate_array(a);
    __goblint_check(xs[0] == 0); //UNKNOWN!
    return 0;
}
