// PARAM: --sets solver td3 --enable ana.int.interval --disable ana.int.def_exc --set ana.activated "['base','mallocWrapper']"
// This is a pattern we saw in some examples for SVCOMP, where instead of the assert(0) there was a call to verifier error.
// Because of the demand-driven nature of our solvers, we never looked at the code inside fail since there is no edge from the loop to the endpoint of f.
// However, the assert(0) (verifier error) is still reachable from main.
void f(void) {
    fail:
        assert(0); //FAIL
        goto fail;
}

int main(void) {
    int top;

    if(top) {
        f();
    }
}
