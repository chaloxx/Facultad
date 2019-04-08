#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <pthread.h>
#include <semaphore.h>

#define N_FILOSOFOS 5
#define ESPERA 5

sem_t semaforo;
pthread_mutex_t tenedor[N_FILOSOFOS];
void pensar(int i)
{
printf("Filosofo %d pensando...\n",i);
usleep(random() % ESPERA);
}
void comer(int i)
{
printf("Filosofo %d comiendo...\n",i);
usleep(random() % ESPERA);
}



void tomar_tenedores(int i)
{
	pthread_mutex_lock(&tenedor[i]);  /*Toma el tenedor a su derecha */
	pthread_mutex_lock(&tenedor[(i+1)%N_FILOSOFOS]);  /*Toma el tenedor a su izquierda */
}

void dejar_tenedores(int i)
{ 
	pthread_mutex_unlock(&tenedor[i]);  /*Deja el tenedor de su derecha */
	pthread_mutex_unlock(&tenedor[(i+1)%N_FILOSOFOS]);  /*Deja el tenedor de su izquierda */
  
  }


void *filosofo(void *arg)
{
int i = (int)arg;
for (;;)
{
sem_wait(&semaforo);
tomar_tenedores(i);
comer(i);
dejar_tenedores(i);
sem_post(&semaforo);
pensar(i);
}
}
int main()
{
	int i;
	sem_init(&semaforo,0,N_FILOSOFOS-1);
	pthread_t filo[N_FILOSOFOS];
	for (i=0;i<N_FILOSOFOS;i++)
		pthread_mutex_init(&tenedor[i], NULL);
	for (i=0;i<N_FILOSOFOS;i++)
		pthread_create(&filo[i], NULL, filosofo, (void *)i);
	pthread_join(filo[0], NULL);
	sem_destroy(&semaforo);
	return 0;
}
