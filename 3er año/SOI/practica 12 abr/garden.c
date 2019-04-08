#include <stdio.h>
#include <pthread.h>

#define N_VISITANTES 1000

int visitantes = 0;

void *molinete(void *arg)
{
  int i;
  #pragma omp parallel for 
  for (i=0;i<N_VISITANTES;i++){
    visitantes++;
    printf("i:%d, core:%d\n", i, omp_get_thread_num());
  }
}

int main()
{
	/*
  #pragma omp parallel
  #pragma omp sections
  {
  #pragma omp section
	molinete(NULL);
  #pragma omp section
	molinete(NULL);
  }
  */
  molinete(NULL);
  printf("Hoy hubo %d visitantes!\n", visitantes);
  return 0;
}
