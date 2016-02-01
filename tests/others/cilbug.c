void f(int a);
void f(int c){
	int x = c;
}
void f(int b);

int main(){
	// cil bug: f.sformals now contains a/b instead of c? -> not the case...
	f(1);
	return 0;
}
