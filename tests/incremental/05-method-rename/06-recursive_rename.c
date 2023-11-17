void foo(int x) {
    if(x > 1) foo(x - 1);
}

int main() {
    foo(10);
}
