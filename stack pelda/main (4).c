#include <stdio.h>
#include <stdlib.h>
#include "Stack.h"

int main()
{
    int n,x;
    printf("n=");
    scanf("%d",&n);
    STACK*A=Creat(n);
    for(int i=0;i<n;++i)
    {
        x=x+rand()%10;
        Push(A,x);
    }
    Print(A);
    printf("%i",Top(A));
    printf("%i",Pop(A));
    Print(A);


    return 0;
}
