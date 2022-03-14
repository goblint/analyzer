// PARAM: --set exp.unrolling-factor 5
#include<assert.h>

int main(void) {
    int i = 0;
    int j = 0;

    while(i< 17) {
        if (j==0) { j=1;  break; }
        i++;
    }

    assert(i == 0);
}
