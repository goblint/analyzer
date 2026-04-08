// CRAM PARAM: --enable witness.yaml.enabled --set witness.yaml.invariant-types '["location_invariant"]' --disable witness.invariant.all-locals
int main() {
  int x;
  x = 5;
  {
    int y;
    y = 10;
  }
  return 0;
}
