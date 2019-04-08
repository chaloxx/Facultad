#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
//1)El problema es que cuando un proceso lee un valor que dejo otro proceso
//puede escribir un valor que no le corresponde
//2)Si N_VISITANTES es igual a 10 y al ejecutar el programa visitantes llega a 
//20 es porque los procesos no estuvieron sincronizados en ningún momento.
//3)El mínimo valor es 2.



#define N_VISITANTES 10000

int visitantes = 0;
pthread_mutex_t s=PTHREAD_MUTEX_INITIALIZER;

void *molinete(void *arg)
{
  int i;
  for (i=0;i<N_VISITANTES;i++){
    //pthread_mutex_lock(&s);
    visitantes++;
    printf("Hay %d visitantes\n",visitantes);
    //pthread_mutex_unlock(&s);
    }
}

int main()
{ 
  pthread_t m1, m2;
  pthread_create(&m1, NULL, molinete, NULL);
  pthread_create(&m2, NULL, molinete, NULL);
  pthread_join(m1, NULL);
  pthread_join(m2, NULL);
  printf("Hoy hubo %d visitantes!\n", visitantes);
  return 0;
}
