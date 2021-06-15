//postinkrement unutar izraza i kao poseban iskaz, primenjen na promenljive i parametre
//RETURN: 2
int funk(int d){
	d++;
	return 0;
	}
int main(){
	int a=2,b=3,c=4;
	a++;
	a = b + c++ - 5;
	return a;
	}
