// PARAM: --set ana.activated[+] apron --set ana.apron.domain polyhedra --set sem.int.signed_overflow assume_none
int main(){
    int x = 0;
    int y = 0;

    while (1) {
        if (x <= 50) y++;
        else y--;

        if (y < 0) break;

        x++;
    }

    return 0;
}