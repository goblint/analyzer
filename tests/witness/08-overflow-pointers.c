void main() {
    int y = 1;
    int *p = &y;
    int z = *p + 2147483646;
    y = 5;
    int x = *p + 2147483646;
}