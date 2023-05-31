extern void abort(void);
void reach_error(){}


int main() {
    int a, b;
    int *p1 = &a;
    int *p2 = &a; // same

    if (p1 != p2) { // negated
        goto ERROR;
    }

    return 0;

    ERROR: {reach_error();abort();}
    return 1;
}
