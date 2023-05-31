// PARAM: --set exp.unrolling-factor 1 --enable dbg.run_cil_check
int main() {
  int m;

  switch (m)
  {
      default:
      do { } while (0);
  }


  goto lab;

  lab: do { } while (0);

  return 0;
}
