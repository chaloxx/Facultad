#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#define N 10

int A[N][N],B[N][N],C[N][N] ,C2[N][N];

void mult(int A[N][N], int B[N][N], int C[N][N])
{
	int i, j, k, temp;
	for (i=0;i<N;i++)
		for (j=0;j<N;j++){
			temp = 0;
			#pragma omp parallel for reduction (+:temp)
			for(k=0;k<N;k++) 
				temp+= A[i][k]*B[k][j];
			C[i][j] = temp;
			}
			
}


void mult2(int A[N][N], int B[N][N], int C[N][N])
{
	int i, j, k;
	for (i=0;i<N;i++)
		for (j=0;j<N;j++)
			for (k=0;k<N;k++)
				C[k][i] += A[k][j]*B[j][i];
}


void imprimir(){
	int i,j;
	for( i = 0; i<N;i++){
		for(j = 0; j<N; j++)
			printf("%d ",C[i][j]);
	
	puts("");
	}
	}


void imprimir2(){
	int i,j;
	for( i = 0; i<N;i++){
		for(j = 0; j<N; j++)
			printf("%d ",C2[i][j]);
	
	puts("");
	}
	}



int main()
{
	srand(time(NULL));
	int i, j;
	for (i=0;i<N;i++)
		for (j=0;j<N;j++) {
			A[i][j] = random() % 10;
			B[i][j] = random() % 10;
		}
	mult(A, B, C);
	mult2(A, B, C2);
	imprimir();
	printf("\n");
	imprimir2();
	
	return 0;
}

