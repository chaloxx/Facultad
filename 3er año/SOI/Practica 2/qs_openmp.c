#include <stdio.h>
#include <stdlib.h> 
#include <pthread.h>        

#define N 1000000
typedef struct {
    int *v;
    int b, t;
} qsparams;

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
 
void Quicksort(int* v, int b, int t)
{
    if(b < t){
            int pivote=colocar(v, b, t),lim1 = pivote-1, lim2=pivote+1;
            if (b < lim1 && lim2 >= t){
                Quicksort(v, b, lim1);
        
            }
            else if (b >= lim1 && lim2 < t){
                Quicksort(v, lim2, t);
            }
            else {
                #pragma omp parallel 
                {
                    #pragma task
                    Quicksort(v, b, lim1);
                    #pragma task
                    Quicksort(v, lim2, t);

                }

            }
    }  
}

int main(int argc, char **argv) 
{
    int *a,i;
    pthread_t t;

    a = malloc(N*sizeof(int));

    for(i=0;i<N;i++) 
        a[i]=random()%N;

   Quicksort(a,0,N-1);
    free(a);
    return 0;
}