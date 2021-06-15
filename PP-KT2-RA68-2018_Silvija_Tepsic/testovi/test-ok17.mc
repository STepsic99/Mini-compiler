//ternarni operator
//RETURN: 7
int main(){
	int b=4,a=3;
	a = (a == b) ? a : 0;
	a = a + (a == b) ? a : b + 3;
	return a;
}
