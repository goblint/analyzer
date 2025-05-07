// PARAM: --set witness.yaml.entry-types[*] location_invariant --set witness.yaml.validate 03-int-log-short.yml

int main() {
  int r;
  int x, y;
  x = 1;
  y = 0;
  ; // SUCCESS (witness)
  ; // SUCCESS (witness)
  return 0;
}
