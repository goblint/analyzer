void f() { }
void g() { }
void h() { }

int main()
{
  int tmp,a,b;
  f(); // fine
  while(a){ // loops with > 1 edges are also fine
    g();
    tmp = 0;
  }
  while(b){ // but side effects of selfloops/reflexive edges get lost somehow...
    h();
  }
  return 0;
}
