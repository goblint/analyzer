// SKIP NONTERM PARAM: --set "ana.activated[+]" termination --set ana.activated[+] apron --enable ana.int.interval --set ana.apron.domain polyhedra
void troll(void (*f) ())
{
    f(f);
}

int main()
{
    troll(troll);
}
