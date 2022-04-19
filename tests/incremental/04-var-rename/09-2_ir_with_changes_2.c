void foo() {
    int fooTwo = 1;
    fooTwo++;
    assert(fooTwo == 2);
}

void bar() {
    int barTwo = 10;
    if (barTwo < 11) barTwo = 20;
    assert(barTwo == 20);
}

int main() {
    foo();
    bar();
    return 0;
}
