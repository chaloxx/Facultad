#include <stdio.h>
#include <stdlib.h>

#define N 200

int A[N][N],B[N][N],C[N][N];

void mult(int A[N][N], int B[N][N], int C[N][N])
{
    int i, j, k;
    for (i=0;i<N;i++) 
        for (j=0;j<N;j++) 
            for (k=0;k<N;k++) 
                C[k][i] += A[k][j]*B[j][i];
}

int main()
{
    int i, j;
    for (i=0;i<N;i++)
        for (j=0;j<N;j++) {
            A[i][j] = random() % 10;
            B[i][j] = random() % 10;
        }

    mult(A, B, C);

    return 0;
}