// SKIP TERM PARAM: --enable ana.wp_run 
int f(int a, int b);
int x, y, u, v;

int main(){
  int x = 1;
  int y = 2;
  int u = f (x, y);

  x = 10;
  y = 20; // only this assignment yields a warning, as the second call of f does not use y
  int v = f (x, y); 

  return u + v;
}
int f(int a, int b) {
  if (a < 5) {
    return a + b;
  } else {
    return a;
  }
}

// this example demonstrates the advantage of forward-contexts. The precise, context-sensitive results of 
// the forward value analysis allow for this analysis to determine that y is only used in the first call of f  