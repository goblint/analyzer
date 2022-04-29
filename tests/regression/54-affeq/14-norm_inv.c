//SKIP PARAM: --set ana.activated[+] affeq  --set ana.matrix "list" --sem.int.signed_overflow "assume_none"  --enable ana.int.interval
// Snippet from https://github.com/sosy-lab/sv-benchmarks/blob/master/c/nla-digbench/hard2.c
// Normalization should be triggered when an invertible expression is assigned.
int main() {
    int A, B;
    int r, d, p, q;
    B = 1;

    r = A;
    d = B;
    p = 1;
    q = 0;

    while (1) {
        if (!(r >= d)) break;

        d = 2 * d;
        p = 2 * p;
    }

    while (1) {

        if (!(p != 1)) break;

        d = d / 2;
        p = p / 2;
        if (r >= d) {
            r = r - d;
            q = q + p;
        }
    }
    return 0;
}