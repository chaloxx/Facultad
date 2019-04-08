#include <stdio.h>
#include <stdlib.h> 
#include <time.h>

#define N 100000000

void swap(int *v, int i, int j) 
{
    int tmp=v[i];
    v[i]=v[j];
    v[j]=tmp;
}

int colocar(int *v, int b, int t)
{
    int i;
    int pivote, valor_pivote;
    int temp;

    pivote = b;
    valor_pivote = v[pivote];
    for (i=b+1; i<=t; i++){
        if (v[i] < valor_pivote){
            pivote++;    
            swap(v,i,pivote);
        }
    }
    temp=v[b];
    v[b]=v[pivote];
    v[pivote]=temp;
    return pivote;
} 
 
void QuicksortSeq(int* v, int b, int t)
{
    int pivote;
    if(b < t){
        pivote=colocar(v, b, t);
        QuicksortSeq(v, b, pivote-1);
        QuicksortSeq(v, pivote+1, t);
    }  
}

int main(int argc, char **argv) 
{
    int *a,i;
    a = malloc(N*sizeof(int));

    for(i=0;i<N;i++) 
        a[i]=random()%N;
    QuicksortSeq(a,0,N-1);
    free(a);
    return 0;
}