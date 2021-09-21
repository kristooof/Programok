#ifndef STACK_H_INCLUDED
#define STACK_H_INCLUDED
#include <stdbool.h>

typedef struct{
    int kapacitas;
    int*elements;
    int sp;
}STACK;

STACK*Creat(int);
void Destroy(STACK*);
void Print(STACK*);
void Push(STACK*,int);
int Pop(STACK*);
int Top(STACK*);
bool IsEmpty(STACK*);
bool IsFull(STACK*);

#endif // STACK_H_INCLUDED
