void foo() {
    int fooOne = 1;
    fooOne++;
    assert(fooOne == 2);
}

void bar() {
    int barOne = 10;
    if (barOne < 11) barOne = 20;
    assert(barOne == 20);
}

int main() {
    foo();
    bar();
    return 0;
}
