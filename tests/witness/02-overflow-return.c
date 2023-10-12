// PARAM: --set ana.activated[+] "localTraces"

int subFunc(int x) {
    return x + 2147483647;
}

void main() {
    subFunc(5);
}