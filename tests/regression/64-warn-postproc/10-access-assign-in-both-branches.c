// PARAM: --enable ana.arrayoob --enable ana.warn-postprocess.enabled

void main() {
    int arr[5], tmp=1,i=0,r0,r1,r2,r3,r4;

    if(r0) {
        if(r1) {
            i = r2; // WARN
        } else {
            i = r3;
        }
        // assert (0 <= i <= 4)
        tmp = 0;
    } else {
        tmp = r4;
    }

    if (i < tmp)
        arr[i] = 0;
    else
        arr[i] = 1;
}