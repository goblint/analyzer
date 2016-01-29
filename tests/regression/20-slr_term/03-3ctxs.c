
int f(int j){
	return j+1;
}

int main(){
	int i = 0;
	while (i<10){
		i = 1 + f(i);
	}
	return i;
}
