// PARAM: --enable ana.int.enums --set ana.activated[+] unassume --set witness.yaml.unassume 33-base-unassume-lor-enums.yml

int main() {
  int i;
  i = 0;

  __goblint_check(i == 0); // UNKNOWN (intentional by unassume)
  __goblint_check(i == 0 || i == 1 || i == 2);
  return 0;
}
