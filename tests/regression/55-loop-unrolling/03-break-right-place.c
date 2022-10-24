// PARAM: --set exp.unrolling-factor 5
#include <goblint.h>

int main(void) {
    int i = 0;
    int j = 0;

    while(i< 17) {
        if (j==0) {
            j = 1;
            // the break is not just out of this unrolling, but out of the entire loop!
            break;
        }
        i++;
    }

    __goblint_check(i == 0);
}
