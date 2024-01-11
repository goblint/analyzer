int main(void) {
    int x = 0;
    int y = x;
    asm ("mov $1, %0" : "=x" (x));
    return 0;
}
