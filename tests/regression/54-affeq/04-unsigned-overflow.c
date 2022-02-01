// PARAM: --set ana.activated[+] affeq --enable ana.int.interval

int main() {
    //Overflow
    int c = 2147483647;
    c = c + 1;

   assert (c < 2147483647); //UNKNOWN!

   int x = c * 2;

   assert (c < 2147483647); //UNKNOWN!

}