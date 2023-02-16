// PARAM: --enable ana.warn-postprocess.enabled --enable ana.arrayoob

void main() {
    int arr[5],i;

    if (i > 3) {
        arr[i]=1; // WARN
    } else {
        arr[i]=2;
    }

}
