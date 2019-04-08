#include <stdio.h>
#include <unistd.h>
#include "synch.hh"
//1)El problema es que cuando un proceso lee un valor que dejo otro proceso
//puede escribir un valor que no le corresponde
//2)Si N_VISITANTES es igual a 10 y al ejecutar el programa visitantes llega a
//20 es porque los procesos no estuvieron sincronizados en ningún momento.
//3)El mínimo valor es 2.



#define N_VISITANTES 2000
#define NUM_THREADS 2

Lock *g;
int visitantes = 0;

void molinete(void * name_)
{
  int i;
  char *name  = (char *) name_;
  for (i=0;i < (N_VISITANTES+1);i++){
    //g-> Acquire();
    visitantes++;
    printf("Visitante Nro: %d soy hilo %s\n",i,name);
    currentThread->Yield();
    //g-> Release();
    }
}




void test_lock(){
  g = new Lock(NULL);
  char **names = new  char* [NUM_THREADS];
  char v[10];
  Thread *threads[NUM_THREADS];

  for (int i = 0; i < NUM_THREADS-1; i++){

    sprintf(v,"hilo::%d",i);
    names[i] = new char [64];
    strcpy(names[i],v);
    threads[i] = new Thread(names[i]);
    threads[i]->Fork(molinete,(void *) names[i]);

  }

 g-> ~Lock();


}
