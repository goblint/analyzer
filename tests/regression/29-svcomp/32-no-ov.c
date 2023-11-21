// PARAM: --enable ana.int.interval --enable ana.sv-comp.enabled --enable ana.sv-comp.functions --set ana.specification "CHECK( init(main()), LTL(G ! overflow) )"

int main(){
	// This is not an overflow, just implementation defined behavior on a cast
	int data = ((int)(rand() & 1 ? (((unsigned)rand()<<30) ^ ((unsigned)rand()<<15) ^ rand()) : -(((unsigned)rand()<<30) ^ ((unsigned)rand()<<15) ^ rand()) - 1));
	return 0;
}