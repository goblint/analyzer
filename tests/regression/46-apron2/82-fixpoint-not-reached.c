// SKIP PARAM: --set sem.int.signed_overflow assume_none --set ana.activated[+] apron

int main() {
 int minInt = -2147483647 + -1;
 int x = (minInt + -1) +1;
 return 0;
}