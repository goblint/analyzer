// SKIP

// Can be used to try timeouts.
// With defaults this will create |int| contexts and lead to a stack overflow in
// 1m24s (8MB default stack size) |rho|=36951 |called|=12320 |contexts|=6161
// 8m48s (16MB, ulimit -Ss 16384) |rho|=73959 |called|=24656 |contexts|=12329
// ? (32MB, ulimit -Ss 32768)
// ? (48MB, ulimit -Ss 49152)
// on a 2015 MacBook Pro at commit 3690e97b.

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
