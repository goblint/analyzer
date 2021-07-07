// from SV-COMP: nla-digbench-scaling/ps6-ll_valuebound5.c
// contains deep integer expressions that shouldn't cause extremely exponential slowdown
// when evaluated by base's eval_rv and EvalInt jointly
// runs (as unknown) under 0.1s

#include <assert.h>
void assume_abort_if_not(int cond) {
  if(!cond) {abort();}
}

int main() {
    short k;
    long long y, x, c;
    assume_abort_if_not(k>=0 && k<=5);
    assume_abort_if_not(k <= 256);

    y = 0;
    x = 0;
    c = 0;

    while (1) {
        assert(-2*y*y*y*y*y*y - 6 * y*y*y*y*y - 5 * y*y*y*y + y*y + 12*x == 0);

        if (!(c < k))
            break;

        c = c + 1;
        y = y + 1;
        x = y * y * y * y * y + x;
    }

    assert(-2*y*y*y*y*y*y - 6 * y*y*y*y*y - 5 * y*y*y*y + y*y + 12*x == 0);
    assert(k*y == y*y);
    return 0;
}
