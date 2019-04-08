#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>
#include <unistd.h>
#include <semaphore.h>
#define N 200
#define S 5 //Sillas libres
#define DELAY (sleep(random()%2))
#define DELAY2 (sleep(random()%7))

int sillasLibres = S, clientePagando,sillonOcupado;
pthread_mutex_t barberoM,clienteM;
pthread_cond_t pagar = PTHREAD_COND_INITIALIZER,corte = PTHREAD_COND_INITIALIZER, despertar =PTHREAD_COND_INITIALIZER;
sem_t semaforo;

void cortando(){
   while(!sillonOcupado)
     pthread_cond_wait(&corte,&barberoM);
   printf("Cortando...\n");
   int i = 0;
   for(i = 0; i < 3;i++){
        printf("8<\n");
        DELAY;
   }
    
   sillonOcupado = 0;
   pthread_cond_signal(&corte);
}



void me_cortan(){
   sillonOcupado = 1;//Se ocupa el sillon
   pthread_cond_signal(&corte);
   while(sillonOcupado)
     pthread_cond_wait(&corte,&barberoM);
}

void pagando(){
    while(!clientePagando)
        pthread_cond_wait(&pagar,&clienteM);
    printf("Pagando...\n");
    for (int i = 0; i < 2; ++i){
       printf("$\n");
       DELAY; 
    }
    clientePagando = 0;
    pthread_cond_signal(&pagar);
}


void me_pagan(){
    clientePagando = 1;
    pthread_cond_signal(&pagar);
    while(clientePagando)
       pthread_cond_wait(&pagar,&barberoM);
}




void* barbero(void* arg){
    int i = 0;
    pthread_mutex_lock(&barberoM);
    for(;;){
        while(sillasLibres == S)
            pthread_cond_wait(&despertar,&barberoM);//Dormir si todas las silla estan libres
        sillasLibres++;//Un cliente se sienta en el sillon dejando una silla de espera libre
        cortando();//Cortar
        me_pagan();//Esperar el pago
    }
    return arg;
}




void* cliente(void* arg){
    if(sillasLibres > 0){
        int id = *(int*)arg;
        printf("Entra cliente %d\n",id);
        sillasLibres--;//El cliente se sienta
        printf("Quedan %d sillas libres\n",sillasLibres);
        pthread_cond_signal(&despertar);//Llamar al barbero por si esta durmiendo
        sem_wait(&semaforo);//Se atiende de a un cliente a la vez
        printf("El cliente %d esta siendo atendido\n",id);
        pthread_mutex_lock(&clienteM); 
        me_cortan();//Esperar que me corten
        pagando();//Pagar
        printf("El cliente %d se retira\n",id);
        pthread_mutex_unlock(&clienteM);
        sem_post(&semaforo);
    }
    return arg;
}





int main(){
  srand(time(NULL));
  int cant,i = 0, arg[N];
  pthread_t b,c[N];
  sem_init(&semaforo,0,1);
  pthread_create(&b,NULL,barbero,NULL);
  for(;i < N; i++){
    arg[i] = i;
    pthread_create(&c[i],NULL,cliente,&arg[i]);
    DELAY2;
  }
    
  pthread_join(b,NULL);
  pthread_join(c[0],NULL);
  
}