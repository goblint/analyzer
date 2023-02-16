// SKIP PARAM: --enable ana.warn-postprocess.enabled --enable ana.arrayoob

void main() {
    int arr[5],tmp,i,r1,r2,r3;

    i = r1;

    if(r2) {
        i = r3; // WARN
        tmp = arr[i];
    } else {
        tmp = arr[i]; // WARN
    }
    tmp = 1;
}