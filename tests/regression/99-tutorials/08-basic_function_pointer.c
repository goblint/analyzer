// SKIP TERM PARAM: --enable ana.wp_run 

int f(int a, int b) {
  return a + 1;
}

int g(int a, int b) {
  return b + 1;
}

int main(){
  int x = 1;
  int y = 2; // this assignment should yield a warning, as y is not used in the path taken in the called function
  
  int (*h) (int, int) = &g;
  h = &f;

  int z = (*h)(x, y);
  return z;
}

// this example demonstrate that the live variable analysis uses flow-sensitive points-to information 