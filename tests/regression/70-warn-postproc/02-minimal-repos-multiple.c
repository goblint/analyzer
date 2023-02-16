// PARAM: --enable ana.warn-postprocess.enabled --enable ana.arrayoob

void main() {
    int arr[5],i=0,r1,r2,tmp1,tmp2;

    i = r1; // WARN
    tmp1 = arr[i];

    //i = r2;
    tmp2 = arr[i];
}