// PARAM: --enable dbg.debug --enable ana.int.interval

int main() {
    int i;
    for (i = 0; i < 5; i++);
    for (i = 5; i > 0; i--);
    for (i = 0; i < 5; i--);
    return 0;
}