#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>
#define N 100000000

//Conclusion: para arreglos de mas de 1000 arreglos conviene

pthread_mutex_t  esperar;

int semaforo = 3;

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
void *Quicksort(void *p)
{
	qsparams *params = (qsparams *)p;
	int *v = params->v;
	int b = params->b;
	int t = params->t;
	int pivote;
	if(b < t){
		pivote=colocar(v, b, t);
		qsparams params1, params2;
		params1.v = v;
		params1.b = b;
		params1.t = pivote-1;
		params2.v = v;
		params2.b = pivote+1;
		params2.t = t;
		if (semaforo != 0){
			pthread_mutex_lock(&esperar);
            printf("%d\n",semaforo);
			semaforo--;
			pthread_mutex_unlock(&esperar);
			pthread_t t1, t2;
			pthread_create(&t1, 0, Quicksort, (void *)&params1);
			pthread_create(&t2, 0, Quicksort, (void *)&params2);
			pthread_join(t1, NULL);
			pthread_join(t2, NULL);
			}
		else{
			 Quicksort((void *)&params1);	
			 Quicksort((void *)&params2);
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
	qsparams params;
	params.v = a;
	params.b = 0;
	params.t = N-1;
	pthread_create(&t, 0, Quicksort, (void *)&params);
	pthread_join(t, NULL);
	free(a);
	return 0;
}
