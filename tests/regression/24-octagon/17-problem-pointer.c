// PARAM: --sets solver td3 --set ana.activated "['base','threadid','threadflag','mallocWrapper']"
void change(int *p) {
    (*p)++;
}

int g;
int main() {
    int c = rand();
    g = 3; // Globals are not tracked by octApron for now
    assert(g != 3); // FAIL 
    assert(g == 3);
    int a = 5;
    int *p = &a; // after this octApron should put a to top because pointers are not tracked
    change(p);
    assert(a - 6 == 0); 
    return 0;
}