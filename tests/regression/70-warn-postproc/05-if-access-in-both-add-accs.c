// PARAM: --enable ana.warn-postprocess.enabled --enable ana.arrayoob

void main() {
    int arr[5],i,r;

    arr[i]=0; // WARN

    if (i > 3) {
        arr[i]=1;
    } else {
        arr[i]=2;
    }

    arr[i]=3;

}
