//default naredba se ne pojavljuje posle svih
int main(){
int a=2;
int b=1;
check (a){
default =>
a = a + b;
case 1 =>
a = a + 5;
finish;
case 5 =>
{
b = 3;
}
}
        return 0;
        }

