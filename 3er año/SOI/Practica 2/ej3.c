#include <omp.h>
#include <stdio.h>
#include <time.h>
#include <stdlib.h>

#define N 500000000
int arreglo[N];

int minimo2(int arreglo[N])
{	
	int i, minimo=arreglo[0];
	#pragma omp parallel for reduction(min:minimo)
	for (i=1; i<N; i++)
		if (minimo > arreglo[i]) minimo=arreglo[i];
	return minimo;
}



int main()
{	
	srand(time(NULL));
	//~ int *arreglo = malloc (sizeof (int) * N);
	//~ for (int i=0; i<N; i++) 
		//~ arreglo[i] = rand();
	printf ("Minimo: %d\n", minimo2(arreglo));
	
	
	return 0;
}
