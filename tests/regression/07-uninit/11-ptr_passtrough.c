// PARAM: --set ana.activated "['base','escape','uninit']"
int* some_function(int * x){
	return x; //NOWARN
}

int main(){
	int z;
	int* zp;
	zp = some_function(&z); //NOWARN
	return z; //WARN
}
