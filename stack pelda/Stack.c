#include <stdio.h>
#include "Stack.h"

STACK * Creat(int n)
{
    STACK * a = (STACK *)calloc(1, sizeof(STACK));
    a->kapacitas = n;
    a->elements = (int *)calloc(n, sizeof(int));
    a->sp=-1;
    return a;
}

void Destroy(STACK*a)
{
    free(a->elements);
    free(a);
}

void Print(STACK*a)
{
    int i;
    for(i=0;i<(a->kapacitas);++i)
    {
        printf("%i ",a->elements[i]);
    }
    printf("\n");
}

void Push(STACK*a, int x)
{
    (a->sp)++;
    a->elements[a->sp]=x;
}

int Pop(STACK*a)
{
    int b=a->elements[a->sp];
    a->sp--;
    return b;
}

int Top(STACK*a)
{
    return a->elements[a->sp];
}

bool IsEmpty(STACK*a)
{
    if((a->sp)==-1)
        return true;
    else
        return false;
}

bool IsFull(STACK*a)
{
    if((a->sp)==(a->kapacitas))
        return true;
    else
        return false;
}
