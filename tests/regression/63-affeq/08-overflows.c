//SKIP PARAM: --set ana.activated[+] affeq  --enable ana.int.interval

int main() {
    //Overflow
    int c = 2147483647;
    c = c + 1;

   __goblint_check(c < 2147483647); //UNKNOWN!

}