// PARAM: --set witness.yaml.validate 02-base-lor-addr.yml

int main() {
  int r; // rand
  int *x, a, b, c, d, e;
  switch (r) {
    case 0:
      x = &a;
      break;
    case 1:
      x = &b;
      break;
    default:
      x = &c;
      break;
  }
  ; // SUCCESS (witness)
  ; // UNKNOWN! (witness)

  int *y;
  switch (r) {
    case 0:
      y = &e;
      break;
    default:
      y = x;
      break;
  }
  ; // UNKNOWN (witness)
  ; // SUCCESS (witness)
  ; // UNKNOWN! (witness)
  return 0;
}
