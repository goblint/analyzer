// PARAM: --enable ana.int.interval --sets solver slr3t --enable exp.no-int-context

int f (int i) { // -2
	return i+1; } // -3
void g(int j) { // -4
	f(j); } // -5
int main(){
	int x;
	x = f(1); // -1
	g(x); // 0
	return 0;
}
