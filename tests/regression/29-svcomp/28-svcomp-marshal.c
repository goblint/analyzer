// SKIP PARAM: --enable ana.sv-comp.enabled --set ana.specification "CHECK( init(main()), LTL(G ! call(reach_error())) )"
// SV-COMP marshaling doesn't work

void f() {

}

int main() {
  f();
  return 0;
}
