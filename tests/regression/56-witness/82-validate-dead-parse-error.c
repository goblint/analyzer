// CRAM PARAM: --set witness.yaml.validate 82-validate-dead-parse-error.yml
// Invariants must also be parsed at dead locations, so that invalid invariants there are reported.

int main() {
  int x = 0;
  if (x) {
    x = 1; // dead
  }
  return 0;
}
