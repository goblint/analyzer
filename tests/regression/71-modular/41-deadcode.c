//PARAM: --set ana.modular.funs "['getndelim2']" --set ana.activated[+] "'modular_queries'" --set ana.activated[+] "'is_modular'" --set ana.activated[+] "'written'" --set ana.activated[+] "'used_globals'" --enable ana.int.interval --enable exp.no-narrow
struct {
  char *a;
} b, c;
char *e;
const char* d() { return b.a; }
void f() { c.a = e; }
void freadseek() { f(); }
void getndelim2() {
  const char* buffer;
  while (1) {
    buffer = d();
    if (buffer)
      ;
    else
      goto g;
    freadseek();
    __goblint_check(1); // should not be dead
  }
g:
}

int main(){
  return 0;
}