#include <stdio.h>
#include <omp.h>
#include <time.h>

/*Operaciones permitidas para reduction:
 * min, max
 * +, -, *
 * &, |, ^, &&, || */

#define N 1000000
int main(){
	
	int i,total,arr[N];
	clock_t t_ini, t_fin,t_ini2,t_fin2;
	double secs;
	for(i = 0; i < N; i++) arr[i] = 1;
	total = 0;
	
	t_ini = clock();
	#pragma omp parallalel for reduction(+=:total)
	{
	for(i = 0; i < N; i++){
		/* #pragma omp critical declara zona critica. Una solucion
		 * mejor fue usar reduction */
		total += arr[i];
		}
	}
	t_fin = clock();
	secs = (double)(t_fin - t_ini) / CLOCKS_PER_SEC;
  	printf("%.16g milisegundos\n", secs * 1000.0);

	
	total = 0;
	t_ini2 = clock();
	for(i = 0; i < N; i++){
		/* #pragma omp critical declara zona critica. Una solucion
		 * mejor fue usar reduction */
		total += arr[i];
		}
	t_fin2 = clock();
	secs = (double)(t_fin2 - t_ini2) / CLOCKS_PER_SEC;
  	printf("%.16g milisegundos\n", secs * 1000.0);

		
	printf("total = %d\n",total);
	
	return 0;
	
	}
