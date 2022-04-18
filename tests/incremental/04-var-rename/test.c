void foo() {
    int i = 0;

    for(int i = 0; i < 10; i++);
}

void bar() {
    int i = 0;
}

int main() {
    foo();
    bar();
    return 0;
}