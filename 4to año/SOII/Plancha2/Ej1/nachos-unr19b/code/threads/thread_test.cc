/// Simple test case for the threads assignment.
///
/// Create several threads, and have them context switch back and forth
/// between themselves by calling `Thread::Yield`, to illustrate the inner
/// workings of the thread system.
///
/// Copyright (c) 1992-1993 The Regents of the University of California.
///               2007-2009 Universidad de Las Palmas de Gran Canaria.
///               2016-2017 Docentes de la Universidad Nacional de Rosario.
/// All rights reserved.  See `copyright.h` for copyright notice and
/// limitation of liability and disclaimer of warranty provisions.


#include "system.hh"
#include "synch.hh"
//#include "garden.c"
#include <stdio.h>


Lock *l;


/// Loop 10 times, yielding the CPU to another ready thread each iteration.
///
/// * `name` points to a string with a thread name, just for debugging
///   purposes.




void
SimpleThread(void *name_)
{
    // Reinterpret arg `name` as a string.
    l-> Acquire();
    char *name = (char *) name_;

    // If the lines dealing with interrupts are commented, the code will
    // behave incorrectly, because printf execution may cause race
    // conditions.
    for (unsigned num = 0; num < 10; num++) {
        printf("*** Thread `%s` is running: iteration %u\n", name, num);
        currentThread->Yield();
    }
    printf("!!! Thread `%s` has finished\n", name);
    l->Release();
}






#define N_VISITANTES 100
#define NUM_THREADS 2

Lock *g;
int visitantes = 0;

void molinete(void * name_)
{
  int i;
  char *name  = (char *) name_;
  for (i=0;i < N_VISITANTES;i++){
    g-> Acquire();
    visitantes++;
    /*char msg[64];
    sprintf(msg,"Visitante Nro: %d soy %s\n",visitantes,name);
    DEBUG('t',msg);
    printf("Visitante Nro: %d soy %s\n",visitantes,currentThread->GetName());*/
    printf("Visitantes : %d soy %s\n",visitantes,name);
    g-> Release();
    //currentThread->Yield();
    }

}




void
ThreadTest()
{
    DEBUG('t', "Entering thread test\n");
    g = new Lock(NULL);
    char *name = new char [64];
    strncpy(name, "2nd", 64);
    Thread *newThread = new Thread(name);
    newThread->Fork(molinete, (void *) name);

    molinete((void *) "1st");
}


























void test_lock(){
  g = new Lock(NULL);
  char **names = new  char* [NUM_THREADS];
  Thread *threads[NUM_THREADS];
  char v[10];

  for (int i = 0; i < NUM_THREADS; i++){
    sprintf(v,"hilo::%d",i);
    names[i] = new char [64];
    strcpy(names[i],v);
    threads[i] = new Thread(names[i]);
    threads[i]->Fork(molinete,(void *) names[i]);

  }

 molinete((void *) currentThread-> GetName());
}



/// Set up a ping-pong between several threads.
///
/// Do it by launching ten threads which call `SimpleThread`, and finally
/// calling `SimpleThread` ourselves.
/*void
ThreadTest()
{
    DEBUG('t', "Entering thread test\n");
    test_lock();

}*/
