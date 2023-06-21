// LOCAL_TERM PARAM: --set "ana.activated[+]" termination --enable warn.debug --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
#include <stdio.h>

void recursiveFunction(int n) {
    // Base case: When n reaches 0, stop recursion
    if (n == 0) {
        printf("Terminating recursion\n");
        return;
    }

    printf("Recursive call with n = %d\n", n);

    // Recursive call: Decrement n and call the function again
    recursiveFunction(n - 1);
}

int main() {
    // Call the recursive function with an initial value
    recursiveFunction(5);

    return 0;
}
