#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>
sem_t tabaco, papel, fosforos, otra_vez;
void agente()
{
	for (;;) {
		int caso = random() % 3;
		sem_wait(&otra_vez);
		switch (caso) {
			case 0:
				sem_post(&tabaco);
				sem_post(&papel);
				break;
			case 1:
				sem_post(&fosforos);
				sem_post(&tabaco);
				break;
			case 2:
				sem_post(&papel);
				sem_post(&fosforos);
				break;
		}
	}
}

void fumar(int fumador)
{
	printf("Fumador %d: Puf! Puf! Puf!\n", fumador);
	//~ sleep(1);
}
void *fumador1(void *arg)
{
	int tmp1=0, tmp2=0;
	for (;;) {
		if( tmp1==1 && tmp2==1 ){
			sem_wait(&tabaco);
			sem_wait(&papel);
			fumar(1);
			sem_post(&otra_vez);
			tmp1 = 0; tmp2 = 0;
		} else {
			sem_getvalue(&tabaco, &tmp1);
			sem_getvalue(&papel, &tmp2);
		}
	}
}
void *fumador2(void *arg)
{
	int tmp1=0, tmp2=0;
	for (;;) {
		if( tmp1==1 && tmp2==1 ){
			sem_wait(&fosforos);
			sem_wait(&tabaco);
			fumar(2);
			sem_post(&otra_vez);
			tmp1 = 0; tmp2 = 0;
		} else {
			sem_getvalue(&fosforos, &tmp1);
			sem_getvalue(&tabaco, &tmp2);
		}
	}
}
void *fumador3(void *arg)
{
	int tmp1=0, tmp2=0;
	for (;;) {
		if( tmp1==1 && tmp2==1 ){
			sem_wait(&papel);
			sem_wait(&fosforos);
			fumar(3);
			sem_post(&otra_vez);
			tmp1 = 0; tmp2 = 0;
		} else {
			sem_getvalue(&papel, &tmp1);
			sem_getvalue(&fosforos, &tmp2);
		}
	}
}
int main()
{
	pthread_t s1, s2, s3;
	sem_init(&tabaco, 0, 0);
	sem_init(&papel, 0, 0);
	sem_init(&fosforos, 0, 0);
	sem_init(&otra_vez, 0, 1);
	pthread_create(&s1, NULL, fumador1, NULL);
	pthread_create(&s2, NULL, fumador2, NULL);
	pthread_create(&s3, NULL, fumador3, NULL);
	agente();
	return 0;
}
