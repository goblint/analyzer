// PARAM: --set ana.activated "['base','threadflag','escape','uninit']"
void some_function(int* x){
	*x = 0;
}

int main(){
	int v;
	some_function(&v);
	return v; //NOWARN
}
