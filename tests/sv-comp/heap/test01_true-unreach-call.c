extern void abort(void);
void reach_error(){}


int main() {
    int a, b;
    int *p1 = &a;
    int *p2 = &b;

    if (p1 == p2) {
        goto ERROR;
    }

    return 0;

    ERROR: {reach_error();abort();}
    return 1;
}
