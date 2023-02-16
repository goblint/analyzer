// PARAM: --enable ana.warn-postprocess.enabled --enable ana.arrayoob

void main() {
    int arr[5],tmp=1,i=0,r0,r1,r2,r3;

    if(r0) {
        if(r1) {
            i = 3;
            tmp = r1;
        } else {
            i = r2; // WARN
        }
        tmp = 0;
    } else {
        tmp = r3;
    }

    if (i < tmp)
        arr[i]=0;
    else
        arr[i]=1;
}