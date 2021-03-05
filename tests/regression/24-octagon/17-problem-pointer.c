// PARAM: --sets solver td3 --set ana.activated "['base','threadid','threadflag','mallocWrapper','octApron']"
void change(int *p) {
    (*p)++;
}

int a;
int main() {
    a = 5;
    int *p = &a;
    change(p);
    assert(a - 6 == 0);
    return 0;
}