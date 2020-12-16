extern void abort(void); 
void reach_error(){}


int q;

int main() {
    int a, b;
    int *p1 = &a;
    int *p2 = p1;

    q = 0;

    if (p1 == p2) {
        goto ERROR;
    }

    return 0;

    ERROR: {reach_error();abort();}
    return 1;
}
