void foo() {
    int fooThree = 1;
    fooThree++;
    assert(fooThree == 2);
}

void bar() {
    int barTwo = 10;
    int x = 3;
    if (x < 11) barTwo = 13;
    assert(x > 1);
}

int main() {
    foo();
    bar();
    return 0;
}
