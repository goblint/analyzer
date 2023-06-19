// PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

int main() {
    int counter = 0;

    while (1) {
        counter++;

        // Dummy code
        printf("Iteration %d\n", counter);
        int result = counter * 2;
        printf("Result: %d\n", result);

        // Condition to terminate the loop
        if (result >= 10) {
            goto end;
        }
    }

end:
    printf("Loop exited. Result is greater than or equal to 10.\n");

    return 0;
}
