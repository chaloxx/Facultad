#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include <unistd.h>

#define N 10
#define MAX 10 /* segundos */

int buffer[N];

int in, out, ctas;


pthread_mutex_t s=PTHREAD_MUTEX_INITIALIZER;

/* colas de bloqueo */
pthread_cond_t lleno=PTHREAD_COND_INITIALIZER, vacio=PTHREAD_COND_INITIALIZER;
/* productor */

void *prod(void *arg)
{
  for(;;){
	sleep(random()%MAX);
	pthread_mutex_lock(&s);
	while(ctas>=N)
	  pthread_cond_wait(&lleno, &s);//Esperar a se haga una modificacion a ctas
	buffer[in]=1;
    in=(in+1)%N;
	ctas++;
	pthread_cond_signal(&vacio); //pthread_cond_broadcast(&vacio); //Avisar que hago una modificacion ctas
	pthread_mutex_unlock(&s);
  }
  return NULL;
}

void *cons(void *arg)
{
  for(;;){
	sleep(random()%MAX);
	pthread_mutex_lock(&s);
	while(ctas<=0)
	  pthread_cond_wait(&vacio,&s);//Esperar a se modifique ctas
	printf("%d\n", buffer[out]);
	out=(out+1)%N;
	ctas--;
	pthread_cond_signal(&lleno);
	pthread_mutex_unlock(&s);
  }
  return NULL;
}

int main()
{
  pthread_t p,c;
  pthread_create(&p, 0, prod, NULL);
  pthread_create(&c, 0, cons, NULL);
  pthread_join(p, NULL);
  pthread_join(c, NULL);
  return 0;
}
