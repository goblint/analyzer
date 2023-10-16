void main() {
    int y = 1;
    int *p = &y;
    *p = 5;
    int x = y + 2147483646;
}