//integer overflow

void main() {
    unsigned short x = 1;
    signed char c = 127, c2 = c, tmp = 2;
    c = (c^c2);
    x = x + 65536; // x becomes 1
    if ( ++x < 2 || ( c == 0) ) {
        int y = 5;
        y = y + 2147483647;
    }
}