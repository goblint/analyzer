// PARAM: --enable ana.warn-postprocess.enabled --enable ana.arrayoob

void main() {
    int a[5], i=0, j=0, tmp=0;

    j=3;
    i=0;
    while (i < a[j]) {
        tmp=2;
        j=10; // WARN
        i++;
        //j = 3;
    }
}