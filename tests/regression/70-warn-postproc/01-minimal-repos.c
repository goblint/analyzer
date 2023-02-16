// PARAM: --enable ana.warn-postprocess.enabled --enable ana.arrayoob

void main() {
    int arr[5],i=0,r;

    i = r; // WARN
    arr[i]=0;
}