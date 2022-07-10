// TEST
//Suma Z+ Con Recursividad code por: sAfOrAs
#include<iostream>
using namespace std;
int leedato()
{
int i;
cin>>i;
return i;
 
}
 
int suma(int a, int b)
{
     
    if(a>0 && b>0)
        return 2+suma(a-1,b-1);
    else
    if(a>0 || b>0)
        return 1+suma(a-1,b-1);
    else
        return 0;
}
 
