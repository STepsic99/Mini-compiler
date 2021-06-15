//konstante u case naredbi nisu istog tipa kao i promenljiva u check_expression
int main(){
int a=2;
int b=1;
check (a){
case 1 =>
a = a + 5;
finish;
case 5u =>
{
b = 3;
}
default =>
a = a + b;
}
return 0;
}

