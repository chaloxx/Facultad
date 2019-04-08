#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include <semaphore.h>


#define N 1
#define ARRLEN 1024
int arr[ARRLEN],ocupadoPorEsc, ocupadoPorLect;

pthread_mutex_t escritorM,lectorM[N];
pthread_cond_t salgo = PTHREAD_COND_INITIALIZER;
sem_t semaforo;





void *escritor(void *arg)
{
    int i, valor;
    int num = *((int *)arg);
    for (;;) {
        sleep(random()%3);
        pthread_mutex_lock(&escritorM);
        sem_getvalue(&semaforo,&valor);
        while(valor < N){//Esperar si hay algun lector o algun escritor
            pthread_cond_wait(&salgo,&escritorM);
            sem_getvalue(&semaforo,&valor);
        }
        ocupadoPorEsc = 1;
        printf("entra escritor %d\n",num);
        for (i=0; i<ARRLEN; i++) {
            arr[i] = num;
            //printf("%d",arr[i]);
            }
        ocupadoPorEsc = 0;
        pthread_cond_signal(&salgo);
        pthread_mutex_unlock(&escritorM);
    }
    return NULL;
}


void *lector(void *arg)
{
    int v, i, err,valor;
    int num = *((int *)arg);
    for (;;) {
        sleep(random()%3);
        pthread_mutex_lock(&lectorM[num]);//Toma su mutex
        while(ocupadoPorEsc)
            pthread_cond_wait(&salgo,&lectorM[num]);
        printf("entra lector %d\n",num);
        sem_wait(&semaforo);
        err = 0;
        v = arr[0];
       
        for (i=1; i<ARRLEN; i++) {
            //printf("%d",arr[i]);
            if (arr[i]!=v) {
                err=1;
                //printf("error %d\n",arr[i]);
                break;
            }
        }
        sem_post(&semaforo);
        pthread_cond_signal(&salgo);
        pthread_mutex_unlock(&lectorM[num]);
        if (err) printf("Lector %d, error de lectura\n", num);
        else printf("Lector %d, dato %d\n", num, v);
    }
    return NULL;
    }

int main()
{
    int i;
    pthread_t lectores[N], escritores[N];
    int arg[N];
    sem_init(&semaforo,0,N);
    for (i=0; i<ARRLEN; i++) {
        arr[i] = -1;
    }
    for (i=0; i<N; i++) {
        arg[i] = i;
        pthread_create(&lectores[i], NULL, lector, (void *)&arg[i]);
        pthread_create(&escritores[i], NULL, escritor, (void *)&arg[i]);
    }
    pthread_join(lectores[0], NULL); /* Espera para siempre */
    sem_destroy(&semaforo);
return 0;
}