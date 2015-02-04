
int main(){
	int x;
	if(x > 1 && x < 10){
		x = 1; // [2,9] ok
	}else{
		x = 3; // [-inf,1], but should be [-inf,1] join [10,inf] = top
	}

	// let's try CIL's output:
	int y;
	if(y > 1){ // line 4
		if(y<10){ // line 4
			y = 1; // line 5
		}else{
			y = 3; // line 7
		}
	}else{
		y = 3; // line 7
	}
	return 0;
}
