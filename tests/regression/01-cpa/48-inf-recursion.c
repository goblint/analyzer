// SKIP

// Can be used to try timeouts.
// With defaults this will create |int| contexts and lead to a stack overflow in
// ~15s (8MB default stack size)
// 1m07s (16MB, ulimit -Ss 16384)
// 6m23s (32MB, ulimit -Ss 32768)
// 16m38s (48MB, ulimit -Ss 49152)
// on a 2015 MacBook Pro.

int f(int x) {
  if (x)
    return f(x+1);
  else
    return x;
}

int main () {
  int a = f(1);

  return 0;
}
