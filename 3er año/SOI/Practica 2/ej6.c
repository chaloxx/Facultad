#include <stdio.h>
#include <pthread.h>
#include <stdlib.h>
#include <semaphore.h>
#include <unistd.h>

#define N 100
#define ARRLEN 1024

enum Q {Lector,Escritor};

int arr[ARRLEN],ocupadoPorEsc, ocupadoPorLect;



pthread_mutex_t sem;
pthread_cond_t esperoLec = PTHREAD_COND_INITIALIZER, esperoEsc = PTHREAD_COND_INITIALIZER, entraEsc = PTHREAD_COND_INITIALIZER;




void lock(enum Q quien)
{
    pthread_mutex_lock(&sem);//Protejer acceso a la region critica
    switch(quien){//Comprobar quien solicita acceso
        case Lector:
            while(ocupadoPorEsc)//El lector no puede entrar si estan escribiendo
                pthread_cond_wait(&esperoEsc,&sem);
            ocupadoPorLect++;
            break;
        case Escritor:
            while(ocupadoPorLect || ocupadoPorEsc)//El escritor no puede entrar si estan escribiendo o leyendo
                pthread_cond_wait(&esperoLec,&sem);
            ocupadoPorEsc++;
            pthread_cond_broadcast(&entraEsc);// Despertar lectores que esperen por la entrada de un escritor
            break;
    }
    pthread_mutex_unlock(&sem);//Desprotejer acceso a region critica
}




void unlock(enum Q quien)
{
    pthread_mutex_lock(&sem);//Proteger region critica
    switch(quien){//Comprobar quien solicita acceso
        case Lector:
            ocupadoPorLect--;
            pthread_cond_broadcast(&esperoLec);//Despertar escritores
            while(!ocupadoPorEsc)//Dormir hasta que ingrese un escritor
                pthread_cond_wait(&entraEsc,&sem);
            break;
        case Escritor:
            ocupadoPorEsc--;
            pthread_cond_broadcast(&esperoEsc);//Despertar lectores
            break;
    }
    pthread_mutex_unlock(&sem);//Desprotejer region critica
}





void *escritor(void *arg)
{
    int i;
    int num = *((int *)arg);
    for (;;) {
        sleep(random()%3);
        lock(Escritor);
        printf("entra escritor %d\n",num);
        for (i=0; i<ARRLEN; i++) {
            arr[i] = num;
            }
        unlock(Escritor);
    }
    return NULL;
}


void *lector(void *arg)
{
    int v, i, err,valor;
    int num = *((int *)arg);
    for (;;) {
        sleep(random()%3);
        lock(Lector);
        printf("entra lector %d\n",num);
        err = 0;
        v = arr[0];
       
        for (i=1; i<ARRLEN; i++) {
            if (arr[i]!=v) {
                err=1;
                break;
            }
        }
        if (err) printf("Lector %d, error de lectura\n", num);
        else printf("Lector %d, dato %d\n", num, v);
        unlock(Lector);
    }
    return NULL;
    }

int main()
{
    int arg[N], i;
    pthread_t escritores[N], lectores[N];
    for (i=0; i<ARRLEN; i++) {
        arr[i] = -1;
    }
    for (i=0; i<N; i++) {
        arg[i] = i;
        pthread_create(&lectores[i], NULL, lector, (void *)&arg[i]);
        pthread_create(&escritores[i], NULL, escritor, (void *)&arg[i]);
    }
    pthread_join(lectores[0], NULL); /* Espera para siempre */
return 0;
}