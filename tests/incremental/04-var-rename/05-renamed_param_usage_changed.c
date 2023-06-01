//This test should mark foo and main as changed

void foo(int a, int b) {
    int x = a;
    int y = b;
}

int main() {
    foo(3, 4);
    return 0;
}