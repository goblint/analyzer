// PARAM: --enable ana.warn-postprocess.enabled --enable ana.arrayoob

void main() {
    int arr[5],i,r0,r1,r2,tmp;

    if(r0) {
        i = r1; // WARN
        // assert (0 <= i <= 4)
        tmp = 0;
    } else {
        tmp = r2;
    }

    if (i > 3) {
        arr[i]=1;
    } else {
        arr[i]=2;
    }

}
