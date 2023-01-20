// PARAM: --enable ana.warn-postprocess.enabled --enable ana.arrayoob

void main() {
    int arr[5],i,r;

    i = r;

    if (i > 3) {
        arr[i]=1; // WARN
    }

}
