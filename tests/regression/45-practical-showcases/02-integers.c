// PARAM: --enable ana.int.interval
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

int main(){
    int input;

    printf("Please input a number between -10 and 10, but not 0:\n");
    scanf("%d", &input);

    // Check input
    if(input == 0 || input > 10 || input < -10){
        printf("Wrong input!\n");
        return EXIT_FAILURE;
    }

    // Compute ...
    int result_mul = input * input;
    assert(result_mul <= 10 * 10);

    int result_div = 1000 / input;
    assert(result_div >= -1000);

    int result_mul_div = result_div * result_div;
    assert(result_mul_div <= 1000000);

    // Printout
    printf("result_mul: %d\n", result_mul);
    printf("result_div: %d\n", result_div);
    printf("result_mul_div: %d\n", result_mul_div);

    return EXIT_SUCCESS;
}
