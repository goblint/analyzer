// ./goblint td3_term.c --sets solver wpoint_simple --enable ana.int.interval --enable dbg.verbose --html --trace sol --enable exp.no-int-context
int f(int x){
  x++;
  return x;
}

int main(){
  int x = 1;
  x = f(x); // entry [1,1]
  x = f(x); // entry [1,2], then prev. [1,3] etc.
  // this terminates for wpoint_simple, but should not terminate for the phased version since it will start narrowing
  return x;
}
