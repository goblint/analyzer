// NOTIMEOUT PARAM: --set exp.unrolling-factor 1000  --set dbg.timeout 15s --enable dbg.timing.enabled --set ana.path_sens '[]'
int main(void) {
  int x = 0;
  for (int i = 0; i < 1000; i++)
    x += i;
  return x;
}
