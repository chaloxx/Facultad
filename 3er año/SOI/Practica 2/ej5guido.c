#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>
#include <assert.h>

#define R 15
#define W 7
#define DELAY (sleep(random()%5))
#define DELAY1 (sleep(random()%45+5))

enum RW {Reader, Writer};

typedef struct {
	int readerin, writerin;
	pthread_mutex_t *sem;
	pthread_cond_t *qr, *qw;
} *RWmutex_t;

#define readlock(s) (rwlock(Reader,s))
#define writelock(s) (rwlock(Writer,s))
#define readunlock(s) (rwunlock(Reader,s))
#define writeunlock(s) (rwunlock(Writer,s))

RWmutex_t rwm;
int val;

RWmutex_t rwlock_init()
{
	RWmutex_t ret;
	ret = malloc(sizeof(*ret));
	ret->readerin=ret->writerin=0;
	ret->sem=malloc(sizeof(*ret->sem));
	pthread_mutex_init(ret->sem,0);
	ret->qr=malloc(sizeof(*ret->qr));
	ret->qw=malloc(sizeof(*ret->qw));
	pthread_cond_init(ret->qr,0);
	pthread_cond_init(ret->qw,0);
	return ret;
}

void rwlock(enum RW que, RWmutex_t m)
{
	pthread_mutex_lock(m->sem);
	switch(que){
		case Reader:
			while(m->writerin)
				pthread_cond_wait(m->qr,m->sem);
			m->readerin++;
			break;
		case Writer:
			while(m->readerin || m->writerin)
				pthread_cond_wait(m->qw, m->sem);
			m->writerin++;
			break;
	}
	pthread_mutex_unlock(m->sem);
}

void rwunlock(enum RW que, RWmutex_t m)
{
	switch(que){
		case Reader:
			pthread_mutex_lock(m->sem);
			m->readerin--;
			pthread_cond_signal(m->qw);
			pthread_mutex_unlock(m->sem);
			break;
		case Writer:
			pthread_mutex_lock(m->sem);
			m->writerin--;
			pthread_cond_broadcast(m->qr);
			pthread_mutex_unlock(m->sem);
			break;
	}
}

void *reader(void *arg)
{
	int quien =*(int*)arg;
	for(;;){
		readlock(rwm);
		printf("%d leyendo\n", quien);
		val=quien;
		DELAY;
		printf("r %d saliendo\n", quien);
		readunlock(rwm);
		DELAY1;
	}
	return NULL;
}

void *writer(void *arg)
{
	int quien=*(int*)arg;
	for(;;){
		writelock(rwm);
		printf("%d escribiendo ......\n", quien);
		val = quien;
		DELAY;
		assert(val==quien);
		printf("w %d saliendo\n", quien);
		writeunlock(rwm);
		DELAY;
	}
	return NULL;
}

int main()
{
	pthread_t r[R], w[W];
	int ar[R], aw[W], i, a=0;
	
	rwm = rwlock_init();
	for(i=0; i<R; i++){
		ar[i]=a++;
		pthread_create(&r[i],0,reader,&ar[i]);
	}
	for(i=0; i<W; i++){
		aw[i]=a++;
		pthread_create(&w[i],0,writer,&aw[i]);
	}
	pthread_join(r[0],NULL);
	return 0;
}
		
