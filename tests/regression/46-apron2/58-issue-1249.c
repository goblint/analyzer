// SKIP PARAM: --set ana.activated[+] apron
int *a;
int b;
void c(int d) {
    // NOCRASH: *a is a null pointer here, so we should warn but maybe not crash
    *a = d;
}
int main() {
    c(b);
}
