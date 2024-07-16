// PARAM: --enable ana.int.interval --set ana.int.refinement once
int main() {
  int i, j;
  if (i < j) //NOWARN
    return 1;
  else
    return 2;
}
