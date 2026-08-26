// PARAM: --enable ana.int.enums --set witness.yaml.validate 01-base-lor-enums.yml

int main() {
  int r; // rand
  int x;
  switch (r) {
    case 0:
      x = 1;
      break;
    case 1:
      x = 3;
      break;
    default:
      x = 6;
      break;
  }
  ; // SUCCESS (witness)
  ; // UNKNOWN! (witness)

  int y;
  switch (r) {
    case 0:
      y = 11;
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
