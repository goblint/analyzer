// function param is renamed (no semantic changes)
// CRAM
void method(int a) {
    int c = a;
}

int main() {
    method(0);
    return 0;
}
