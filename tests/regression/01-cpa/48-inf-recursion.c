// SKIP

// with defaults this will create |int| contexts and practically take forever (how long exactly?)
// can be used to try timeouts

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
