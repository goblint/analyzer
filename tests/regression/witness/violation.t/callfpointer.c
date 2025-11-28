extern void abort(void);
extern void __assert_fail(const char *, const char *, unsigned int, const char *) __attribute__ ((__nothrow__ , __leaf__)) __attribute__ ((__noreturn__));
void reach_error() { __assert_fail("0", "callfpointer.c", 3, "reach_error"); }


void f(void g(int)) {
 g(1);
}

void h(int i) {
 if(i==1) {
  ERROR: {reach_error();abort();}
 } else {

 }
}
int main(void) {
 f(h);

 return 0;
}
